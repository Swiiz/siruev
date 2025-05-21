#![feature(prelude_import)]
#[prelude_import]
use std::prelude::rust_2024::*;
#[macro_use]
extern crate std;
use std::marker::PhantomData;
use siruev::*;
struct EventA<'a>(&'a str);
const _: () = {
    type __Lt = ::higher_kinded_types::ඞ::ForLt<
        dyn for<'a> ::higher_kinded_types::ඞ::WithLifetime<'a, T = EventA<'a>>,
    >;
    impl<'a> siruev::Event<'a> for EventA<'a> {
        type ForLt = __Lt;
    }
};
#[automatically_derived]
impl<'a> ::core::fmt::Debug for EventA<'a> {
    #[inline]
    fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
        ::core::fmt::Formatter::debug_tuple_field1_finish(f, "EventA", &&self.0)
    }
}
#[allow(dead_code)]
struct EventB(&'static str);
const _: () = {
    type __Lt = ::higher_kinded_types::ඞ::ForLt<
        dyn for<'none> ::higher_kinded_types::ඞ::WithLifetime<'none, T = EventB>,
    >;
    impl siruev::Event<'_> for EventB {
        type ForLt = __Lt;
    }
};
#[automatically_derived]
#[allow(dead_code)]
impl ::core::fmt::Debug for EventB {
    #[inline]
    fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
        ::core::fmt::Formatter::debug_tuple_field1_finish(f, "EventB", &&self.0)
    }
}
struct Test<A>(PhantomData<A>);
const _: () = {
    type __Lt<A> = ::higher_kinded_types::ඞ::ForLt<
        dyn for<'none> ::higher_kinded_types::ඞ::WithLifetime<'none, T = Test<A>>,
    >;
    impl<A> siruev::Event<'_> for Test<A> {
        type ForLt = __Lt<A>;
    }
};
#[automatically_derived]
impl<A: ::core::fmt::Debug> ::core::fmt::Debug for Test<A> {
    #[inline]
    fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
        ::core::fmt::Formatter::debug_tuple_field1_finish(f, "Test", &&self.0)
    }
}
fn main() {}
