use proc_macro::TokenStream;
use quote::quote;
use syn::{
    DeriveInput, Expr, ExprAssign, ExprParen, GenericParam, ItemFn, Token, Type,
    parse::{Parse, ParseStream},
    parse_macro_input,
    punctuated::Punctuated,
    token::Paren,
};

#[proc_macro_derive(Event)]
pub fn derive_event(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as DeriveInput);

    let name = &input.ident;
    let generics = &input.generics;
    let (impl_generics, ty_generics, where_clause) = generics.split_for_impl();

    let mut lifetimes = generics.lifetimes();

    let (trait_lifetime, gen_lifetime) = match lifetimes.next() {
        Some(lt) if lifetimes.next().is_none() => (lt.lifetime.clone(), lt.lifetime.clone()),
        None => (
            syn::Lifetime::new("'_", proc_macro2::Span::call_site()),
            syn::Lifetime::new("'none", proc_macro2::Span::call_site()),
        ),
        _ => {
            return syn::Error::new_spanned(
                &generics,
                "Event currently supports at most one lifetime parameter",
            )
            .to_compile_error()
            .into();
        }
    };

    let only_types = generics
        .params
        .iter()
        .filter_map(|param| match param {
            syn::GenericParam::Type(ty) => Some(ty),
            _ => None,
        })
        .collect::<Box<_>>();

    //quote only types generics
    let only_types_generics = (!only_types.is_empty()).then_some(quote! { <#(#only_types),*> });

    let mut where_clause = where_clause.cloned().unwrap_or_else(|| syn::WhereClause {
        where_token: Default::default(),
        predicates: Default::default(),
    });
    for param in &generics.params {
        if let GenericParam::Type(ty) = param {
            let ident = &ty.ident;
            where_clause
                .predicates
                .push(syn::parse_quote!(#ident: 'static));
        }
    }

    quote! {
        const _: () = {
            type __Lt #only_types_generics = siruev::manual::ForLt!(<#gen_lifetime> = #name #ty_generics);
            impl #impl_generics siruev::Event<#trait_lifetime> for #name #ty_generics #where_clause {
                type ForLt = __Lt #only_types_generics;
            }
        };
    }
    .into()
}

/// Marks a function as an event handler for one or more event types.
///
/// ## Usage
///
/// Automatic registration for a single event (inferred from the parameter type):
/// ```rust
/// #[event_handler]
/// fn handle(event: &MyEvent) {
///     println!("{}", event.0);
/// }
/// ```
///
/// Manual registration for one or more events (event types must satisfy the generic bounds):
/// ```rust
/// #[event_handler(EventA, EventB)]
/// fn handle<T: Debug>(event: &T) {
///     println!("{:?}", event);
/// }
/// ```
///
/// ## Priority
///
/// You can also specify a `priority` for the handler function using the syntax:
/// ```rust
/// #[event_handler(priority = 1)]
/// fn handle(event: &MyEvent) {
///     println!("{}", event.0);
/// }
///
///  #[event_handler(priority = 1, EventA, EventB)]
/// fn handle<T: Debug>(event: &T) {
///     println!("{:?}", event);
/// }
/// ```
/// - The priority value controls the order in which handlers are executed: lower values are executed first, and higher values are executed later.
/// - This priority attribute applies to all events in the handler declaration unless overridden.
///
/// ### Event-Specific Priority Overrides
///
/// You can override the priority for individual events by specifying event-specific priorities. For example:
/// ```rust
/// #[event_handler(priority = -1, &EventA(priority = 2), &EventB)]
/// fn handle(event: &dyn Event) {
///     println!("{:?}", event);
/// }
/// ```
/// In this case:
/// - The root `priority = -1` applies to the handler function for any event type not explicitly overridden.
/// - `EventA(priority = 2)` sets a higher priority for `EventA` than the root priority of `-1`.
/// - `EventB` inherits the root priority of `-1` since no override is provided.
///
/// ## Requirements
///
/// - The event type(s) must implement [`Event`](crate::Event).
#[proc_macro_attribute]
pub fn event_handler(attr: TokenStream, item: TokenStream) -> TokenStream {
    let input_fn = parse_macro_input!(item as ItemFn);
    let args = parse_macro_input!(attr as HandlerArgs);
    let fn_name = &input_fn.sig.ident;
    let fn_args = &input_fn.sig.inputs;

    let event_type = match fn_args.first() {
        Some(syn::FnArg::Typed(pat_type)) => pat_type.ty.clone(),
        _ => panic!("Expected a single argument with a reference to event type"),
    };

    let priority = args.priority.as_ref().map_or(quote! { 0 }, |p| {
        let p = &**p;
        quote! { #p }
    });
    if let Some(events) = args.events {
        let events = events.iter().map(|e| {
            let priority = e.priority.as_ref().map_or(priority.clone(), |p| {
                let p = &**p;
                quote! { #p }
            });
            let ty = &e.ty;
            quote! { #ty: #priority }
        });
        quote! {
            #input_fn

            siruev::systems! { #(#events),* => #fn_name }
        }
        .into()
    } else {
        quote! {
            #input_fn

            siruev::systems! { #event_type: #priority => #fn_name, }
        }
        .into()
    }
}

#[derive(Default)]
struct HandlerArgs {
    priority: Option<Box<Expr>>,
    events: Option<Punctuated<HandlerMeta, Token![,]>>,
}

struct HandlerMeta {
    ty: Type,
    priority: Option<Box<Expr>>,
}

impl Parse for HandlerArgs {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let mut meta = HandlerArgs::default();

        if input.peek(syn::Ident) && input.peek2(Token![=]) {
            let ident: syn::Ident = input.parse()?;
            if ident == "priority" {
                input.parse::<Token![=]>()?;
                meta.priority = Some(input.parse()?);
            }
        }

        if !input.is_empty() {
            input.parse::<Token![,]>()?;
            meta.events = Some(Punctuated::<_, Token![,]>::parse_terminated(input)?);
        }

        Ok(meta)
    }
}

impl Parse for HandlerMeta {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let ty: Type = input.parse()?;
        if !input.peek(Paren) {
            return Ok(HandlerMeta { ty, priority: None });
        }
        let ExprParen { expr, .. } = input.parse()?;

        let Expr::Assign(ExprAssign { left, right, .. }) = *expr else {
            panic!(
                "Expression inside parentheses must be an assignment (e.g., `priority = value`)."
            );
        };

        let Expr::Path(syn::ExprPath { path, .. }) = *left else {
            panic!("Left side of the expression is not a valid identifier.");
        };

        let Some(segment) = path.get_ident() else {
            panic!("Left side of the expression is not a valid identifier.");
        };

        if segment != "priority" {
            panic!("Left side of the expression must be `priority`.");
        }

        Ok(HandlerMeta {
            ty,
            priority: Some(right),
        })
    }
}

/// Derives [`State`](crate::State) for a type.
///
/// The type must not be public.            
#[cfg(feature = "state")]
#[proc_macro_derive(State)]
pub fn state_derive(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as DeriveInput);
    let name = &input.ident;

    if matches!(input.vis, syn::Visibility::Public(_)) && input.generics.params.is_empty() {
        return syn::Error::new_spanned(&input.vis, "State shouldn't be public unless it has generics (dependency injection), use events instead")
            .to_compile_error()
            .into();
    }

    quote! {
       impl siruev::manual::State for #name {
            fn into_any(self: Box<Self>) -> Box<dyn std::any::Any> { self }
            fn as_any(&self) -> &dyn std::any::Any { self }
            fn as_any_mut(&mut self) -> &mut dyn std::any::Any { self }
       }
    }
    .into()
}
