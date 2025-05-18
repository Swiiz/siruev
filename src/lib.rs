#![doc = include_str!("../README.md")]

use higher_kinded_types::ForLt;
use itertools::Itertools;
use manual::{Handler, System};
use std::{any::Any, collections::HashMap, sync::LazyLock};

pub use proc_macro::*;

/// Manualy declares which handler functions should be executed in response to specific event types.
///
/// This macro can be called multiple times, but it should not be invoked from within other declarations (such as functions, impl blocks, or other macros). Instead, call it at the root level of your module or application to ensure proper system registration.
///
/// ## Syntax
///
/// The `priority` of the handler can be specified after the event name in the following format:
///
/// ```ignore
/// systems! {
///     A: 1 => handler_fn, // handler_fn for A with priority 1
///     B => another_handler_fn, // handler_fn for B with priority 0
///     A: 2, B: 1 => yet_another_handler_fn, // handler_fn for A with priority 2, handler_fn for B with priority 1
/// }
/// ```
#[macro_export]
macro_rules! systems {
    ($($($event:ty $(: $priority:expr)?),* => $system:expr),*$(,)?) => {
        $(
            $(
                $crate::manual::submit!($crate::manual::Handler {
                    event: $crate::manual::ConstTypeId::of::<$event>(),
                    system: $crate::manual::system::<<$event as $crate::Event>::ForLt>($crate::systems!(__priv $($priority)?), &$crate::manual::fn_cast(|e| $system(e))),
                });
            )*
        )*
    };
    (__priv $priority:expr) => { $priority }; (__priv) => { 0 };
}

/// ⚠️ Should not be used directly unless you know what you're doing.
pub mod manual {
    use super::*;
    /// Macro and trait from [`trait@higher_kinded_types::ForLt`].
    ///
    pub use higher_kinded_types::ForLt;
    pub use inventory::submit;
    pub use typeid::ConstTypeId;

    #[derive(Debug)]
    pub struct System {
        pub(crate) priority: i8,
        pub(crate) callback: &'static (dyn Any + Send + Sync),
    }

    pub const fn fn_cast<T, F: Fn(T)>(x: F) -> F {
        x
    }

    /// Creates a [`System`] from a function pointer.
    pub const fn system<T: ForLt + 'static>(
        priority: i8,
        callback: &'static fn(<T as ForLt>::Of<'_>),
    ) -> System {
        System { priority, callback }
    }

    /// Represents a handler function and the event type it should be called in response to. Can be submitted to the system using (crate::manual::submit).
    pub struct Handler {
        pub event: typeid::ConstTypeId,
        pub system: System,
    }

    inventory::collect!(Handler);
}

/// A marker trait for types that can be emitted .
///
/// Types implementing `Event` can be dispatched using (crate::emit)
/// and received by functions marked with [`#[event_handler]`](crate::event_handler).
///
/// This trait is typically derived:
/// ```rust
/// #[derive(Event, Debug)]
/// struct MyEvent(String);
/// ```
///
/// You can also implement it manually if needed:
/// ```rust
/// impl<'a> Event<'a> for MyType {
///     type ForLt = siruev::manual::ForLt!(MyType);
/// }
/// ```
///
/// ## Lifetimes
/// The trait is generic over a lifetime `'a`, which allows event types
/// to borrow data (e.g., `&'a str`). For `'static` data, use `'static`.
///
/// ## Reference Implementations
/// The trait is also implemented for references to types that implement `Event`,
/// which allows passing events by reference.
///
/// ## See also
/// - [`emit`]: Dispatches an event.
/// - [`#[event_handler]`](crate::event_handler): Registers a function to handle events.
///
/// [`emit`]: crate::emit
pub trait Event<'a> {
    type ForLt: ForLt<Of<'a> = Self> + 'static;
}

static HANDLERS: LazyLock<HashMap<&'static typeid::ConstTypeId, Vec<&'static System>>> =
    LazyLock::new(|| {
        let mut groups = inventory::iter::<Handler>
            .into_iter()
            .map(|h| (&h.event, &h.system))
            .into_group_map();
        groups
            .values_mut()
            .for_each(|v| v.sort_by_key(|s| s.priority));
        groups
    });

fn run_system<'a, T: ForLt + 'static>(sys: &System, event: T::Of<'a>) {
    let callback: &fn(<T as ForLt>::Of<'_>) = sys
        .callback
        .downcast_ref()
        .expect("System doesn't match event");
    (callback)(event);
}

mod sealed_sync {
    use super::*;
    pub fn emit<'a, E: Event<'a> + Clone>(event: E) -> usize {
        HANDLERS
            .get(&typeid::ConstTypeId::of::<<E::ForLt as ForLt>::Of<'a>>())
            .map_or(0, |handlers| {
                return handlers
                    .into_iter()
                    .map(|s| run_system::<E::ForLt>(s, event.clone()))
                    .count();
            })
    }
}

#[cfg(feature = "parallel")]
pub use sealed_sync::emit as sync_emit;

/// Emits an event by value, invoking all registered handlers for its type.
///
/// This function dispatches the given event to all handlers registered for `E`,
/// The event is cloned for each handler invocation, so the type must implement [`Clone`].
///
/// ## Example
/// ```rust
/// emit(MyEvent(42)); // Clone is required
/// ```
/// or
/// ```rust
/// emit(&MyEvent("hello")); // &MyEvent trivially implements Clone
/// ```
///
/// ## Priority
/// Handlers with **lower priority values** (e.g., `priority = -1`) run **first**, while those with **higher priority values** (e.g., `priority = 1`) run **later**.  
/// If multiple handlers have the same priority, their order is undefined.
///
/// ## Parameters
/// - `event`: The event instance to emit. It must implement  and [`Clone`].
///
/// ## Parallel Execution
/// If the `parallel` feature is enabled, handlers may be invoked concurrently
/// using a `rayon`. Handlers must not share mutable state unless properly synchronized.
/// Without this feature, handlers are invoked sequentially in undefined order (other than priority).
///
/// ## Returns
/// The number of handlers that were invoked.
///
/// ## Notes
/// - For `&mut` handlers, use [`emit_mut`].
///
/// ## See also
/// - [`#[event_handler]`](crate::event_handler): Registers handlers.
/// - [`Event`]: Trait implemented by all event types.
///
/// [`Event`]: crate::Event
/// [`emit_mut`]: crate::emit_mut
pub fn emit<'a, E: Event<'a> + Clone>(event: E) -> usize
where
    E: sealed::SyncIfFeature,
{
    #[cfg(not(feature = "parallel"))]
    return sealed_sync::emit(event);

    #[cfg(feature = "parallel")]
    HANDLERS
        .get(&typeid::ConstTypeId::of::<<E::ForLt as ForLt>::Of<'a>>())
        .map_or(0, |handlers| {
            use rayon::iter::ParallelIterator;
            group_by_key(handlers, |h| h.priority)
                .into_iter()
                .fold(0, |count, i| {
                    count
                        + rayon::iter::IntoParallelIterator::into_par_iter(i)
                            .map(|s| run_system::<E::ForLt>(s, event.clone()))
                            .count()
                })
        })
}

/// Emits a mutable event by reference, invoking all registered `&mut` handlers for its type.
///
/// This function dispatches the given event to all handlers registered for `&mut E`,
/// allowing handlers to modify the event in place.
///
/// ## Example
/// ```rust
/// let mut event = MyEvent("hello");
/// emit_mut(&mut event);
/// println!("{}", event.0); // Handler may have mutated it
/// ```
///
/// ## Priority
/// Handlers with **lower priority values** (e.g., `priority = -1`) run **first**, while those with **higher priority values** (e.g., `priority = 1`) run **later**.  
/// If multiple handlers have the same priority, their order is undefined.
///
/// ## Parameters
/// - `event`: A mutable reference to the event. The event type must implement .
///
/// ## Returns
/// The number of handlers that were invoked.
///
/// ## Notes
/// - For read-only, shared access or parallelism, use [`emit`].
///
/// ## See also
/// - [`#[event_handler]`](crate::event_handler): Registers handlers.
/// - [`Event`]: Trait implemented by all event types.
///
/// [`Event`]: crate::Event
/// [`emit`]: crate::emit
pub fn emit_mut<'a, E: Event<'a> + 'static>(event: &mut E) -> usize {
    type Lt<'a, 'b, E> = <&'a mut E as Event<'b>>::ForLt;
    HANDLERS
        .get(&typeid::ConstTypeId::of::<<Lt<E> as ForLt>::Of<'a>>())
        .map_or(0, |handlers| {
            handlers
                .into_iter()
                .map(|s| run_system::<Lt<E>>(s, event))
                .count()
        })
}

impl<'a, T: Event<'a> + 'static> Event<'a> for &'a T {
    type ForLt = ForLt!(&T);
}

impl<'a, T: Event<'a> + 'static> Event<'a> for &'a mut T {
    type ForLt = ForLt!(&mut T);
}

mod sealed {
    #[cfg(feature = "parallel")]
    pub trait SyncIfFeature: Sync {}
    #[cfg(feature = "parallel")]
    impl<T: Sync> SyncIfFeature for T {}

    #[cfg(not(feature = "parallel"))]
    pub trait MaybeSyncIfFeature {}
    #[cfg(not(feature = "parallel"))]
    impl<T> SyncIfFeature for T {}
}

#[cfg(feature = "parallel")]
fn group_by_key<I, F, K, T>(iter: I, key_fn: F) -> Vec<Vec<T>>
where
    I: IntoIterator<Item = T>,
    F: Fn(&T) -> K,
    K: Eq,
{
    let mut result = Vec::new();
    let mut current_group = Vec::new();
    let mut prev_key = None;

    for item in iter {
        let key = key_fn(&item);

        if let Some(prev) = prev_key {
            if prev != key {
                result.push(current_group);
                current_group = Vec::new();
            }
        }

        current_group.push(item);
        prev_key = Some(key);
    }

    if !current_group.is_empty() {
        result.push(current_group);
    }

    result
}
