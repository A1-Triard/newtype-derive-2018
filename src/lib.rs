// Copyright (c) 2015 macro-attr contributors.
// Copyright (c) 2021 Warlock <internalmike@gmail.com>
//
// Licensed under the MIT license (see LICENSE or <http://opensource.org
// /licenses/MIT>) or the Apache License, Version 2.0 (see LICENSE of
// <http://www.apache.org/licenses/LICENSE-2.0>), at your option. All
// files in the project carrying such notice may not be copied, modified,
// or distributed except according to those terms.

#![deny(warnings)]
#![doc(test(attr(deny(warnings))))]
#![doc(test(attr(allow(dead_code))))]
#![doc(test(attr(allow(unused_variables))))]
#![doc(test(attr(allow(unused_macros))))]

//! This crate provides several macros for deriving implementations of various traits for "newtype"
//! wrappers (i.e. tuple structs with a single element).
//! That is, given a tuple struct with exactly one field (e.g. `struct Buckets(i32)`),
//! these macros will derive "obvious" implementations of traits such as
//! `Add`, `Neg`, `Index`, `Deref`, etc.
//!
//! All of these macros are designed to be used with the
//! [`macro-attr-2018`](https://crates.io/crates/macro-attr-2018) crate,
//! though they can be used independent of it.
//!
//! # Example
//!
//! Create a simple integer wrapper with some arithmetic operators:
//!
//! ```rust
//! use macro_attr_2018::macro_attr;
//! use newtype_derive_2018::*;
//!
//! macro_attr! {
//!     #[derive(NewtypeAdd!, NewtypeMul!(i32))]
//!     pub struct Happy(pub i32);
//! }
//!
//! # fn main() {
//! // Let's add some happy little ints.
//! let a = Happy(6);
//! let b = Happy(7);
//! let c = (a + b) * 3;
//! let d: i32 = c.0;
//! assert_eq!(d, 39);
//! # }
//! ```
//!
//! Create a "deref-transparent" wrapper around a smart pointer:
//!
//! ```rust
//! use macro_attr_2018::macro_attr;
//! use newtype_derive_2018::*;
//!
//! macro_attr! {
//!     #[derive(NewtypeDeref!, NewtypeDerefMut!)]
//!     pub struct I32Array(Vec<i32>);
//! }
//!
//! # fn main() {
//! let arr = I32Array(vec![1, 2, 3]);
//! assert_eq!(&*arr, &[1, 2, 3]);
//! # }
//! ```
//!
//! # Overview
//!
//! This crate provides macros to derive implementations of the following traits for newtype structs:
//!
//! - binary arithmetic operators: `Add`, `BitAnd`, `BitOr`, `BitXor`, `Div`, `Mul`, `Rem`,
//!   `Sub`, `Shl`, `Shr`, plus the corresponding `*Assign` traits.
//! - unary arithmetic operators: `Neg`, `Not`.
//! - other operators: `Deref`, `DerefMut`, `Index`, `IndexMut`.
//! - formatting: `Binary`, `Debug`, `Display`, `LowerExp`, `LowerHex`, `Octal`,
//!   `Pointer`, `UpperExp`, `UpperHex`.
//!
//! All of these macros are named `Newtype$Trait`.
//!
//! All these macros support generic newtype structs. By default, no bounds for generic
//! parameters generated. To add constraints, add where clause to the end of macros arguments.
//! For example:
//!
//! ```rust
//! # use core::ops::{Add, Sub};
//! # use macro_attr_2018::macro_attr;
//! # use newtype_derive_2018::{NewtypeAdd, NewtypeSub};
//!
//! macro_attr! {
//!     #[derive(NewtypeAdd!(where T: Add<Output=T>))]
//!     #[derive(NewtypeAdd!(&self, &Self where T: Add<Output=T>))]
//!     #[derive(NewtypeSub!(* where T: Sub<Output=T>))]
//!     pub struct Dummy<T: Copy>(T);
//! }
//! ```
//!
//! ## Binary Arithmetic Operators
//!
//! Each of the binary arithmetic operators accept several deriving forms.
//! To use `Add` on a struct `T` as an example:
//!
//! | Attribute                 | Generated implementation                  |
//! |---------------------------|-------------------------------------------|
//! | `NewtypeAdd`              | `impl Add<T, Output=T> for T`             |
//! | `NewtypeAdd(&self)`       | `impl<'a> Add<T, Output=T> for &'a T`     |
//! | `NewtypeAdd(U)`           | `impl Add<U, Output=T> for T`             |
//! | `NewtypeAdd(&self, U)`    | `impl<'a> Add<U, Output=T> for &'a T`     |
//! | `NewtypeAdd(*)`           | All four combinations of `T` and `&T`     |
//!
//! The `*Assign` variants accept zero or one argument only. For example:
//!
//! | Attribute                 | Generated implementation                  |
//! |---------------------------|-------------------------------------------|
//! | `NewtypeAddAssign`        | `impl AddAssign<T> for T`                 |
//! | `NewtypeAddAssign(U)`     | `impl Add<U> for T`                       |
//! | `NewtypeAddAssign(*)`     | Implements for `T` and `&T`.              |
//!
//! In all cases, the implementation unwraps the newtype (where necessary),
//! forwards to the wrapped value's implementation, then re-wraps the result in the newtype.
//!
//! ## Unary Arithmetic Operators
//!
//! Each of the binary arithmetic operators accept several deriving forms.
//! To use `Neg` on a struct `T` as an example:
//!
//! | Attribute                 | Generated implementation                  |
//! |---------------------------|-------------------------------------------|
//! | `NewtypeNeg`              | `impl Neg<Output=T> for T`                |
//! | `NewtypeNeg(&self)`       | `impl<'a> Neg<Output=T> for &'a T`        |
//! | `NewtypeNeg(*)`           | Both of the above                         |
//!
//! In all cases, the implementation unwraps the newtype,
//! forwards to the wrapped value's implementation, then re-wraps the result in the newtype.
//!
//! ## Other Operators
//!
//! `NewtypeDeref` and `NewtypeDerefMut` only support the argument-less form.
//! The call is forwarded to the wrapped value's implementation.
//!
//! `NewtypeIndex` and `NewtypeIndexMut` must be used as `NewtypeIndex(usize)`,
//! where the argument is the type to use for indexing.
//! The call is forwarded to the wrapped value's implementation.
//!
//! ## Formatting
//!
//! The deriving macros for the formatting traits in [`std::fmt`][core::fmt]
//! forward to the wrapped value's implementation.
//!
//! ## Using Without `macro_attr!`
//!
//! Although designed to be used with
//! [`macro_attr!`](https://docs.rs/macro-attr-2018/*/macro_attr_2018/macro.macro_attr.html),
//! all of the macros in this crate can be used without it.
//! The following:
//!
//! ```rust
//! use macro_attr_2018::macro_attr;
//! use newtype_derive_2018::*;
//!
//! macro_attr! {
//!     #[derive(Copy, Clone, Debug, NewtypeAdd!, NewtypeAdd!(f32))]
//!     pub struct Meters(pub f32);
//! }
//! #
//! # fn main() { }
//! ```
//!
//! can also be written as
//!
//! ```rust
//! use newtype_derive_2018::*;
//!
//! #[derive(Copy, Clone, Debug)]
//! pub struct Meters(pub f32);
//!
//! NewtypeAdd! { () pub struct Meters(pub f32); }
//! NewtypeAdd! { (f32) pub struct Meters(pub f32); }
//! #
//! # fn main() { }
//! ```

#![no_std]

#[doc(hidden)]
pub use core::fmt::Binary as std_fmt_Binary;
#[doc(hidden)]
pub use core::fmt::Debug as std_fmt_Debug;
#[doc(hidden)]
pub use core::fmt::Display as std_fmt_Display;
#[doc(hidden)]
pub use core::fmt::Formatter as std_fmt_Formatter;
#[doc(hidden)]
pub use core::fmt::LowerExp as std_fmt_LowerExp;
#[doc(hidden)]
pub use core::fmt::LowerHex as std_fmt_LowerHex;
#[doc(hidden)]
pub use core::fmt::Octal as std_fmt_Octal;
#[doc(hidden)]
pub use core::fmt::Pointer as std_fmt_Pointer;
#[doc(hidden)]
pub use core::fmt::UpperExp as std_fmt_UpperExp;
#[doc(hidden)]
pub use core::fmt::UpperHex as std_fmt_UpperHex;
#[doc(hidden)]
pub use core::fmt::Result as std_fmt_Result;
#[doc(hidden)]
pub use core::ops::Add as std_ops_Add;
#[doc(hidden)]
pub use core::ops::BitAnd as std_ops_BitAnd;
#[doc(hidden)]
pub use core::ops::BitOr as std_ops_BitOr;
#[doc(hidden)]
pub use core::ops::BitXor as std_ops_BitXor;
#[doc(hidden)]
pub use core::ops::Deref as std_ops_Deref;
#[doc(hidden)]
pub use core::ops::DerefMut as std_ops_DerefMut;
#[doc(hidden)]
pub use core::ops::Div as std_ops_Div;
#[doc(hidden)]
pub use core::ops::Index as std_ops_Index;
#[doc(hidden)]
pub use core::ops::IndexMut as std_ops_IndexMut;
#[doc(hidden)]
pub use core::ops::Mul as std_ops_Mul;
#[doc(hidden)]
pub use core::ops::Neg as std_ops_Neg;
#[doc(hidden)]
pub use core::ops::Not as std_ops_Not;
#[doc(hidden)]
pub use core::ops::Rem as std_ops_Rem;
#[doc(hidden)]
pub use core::ops::Shl as std_ops_Shl;
#[doc(hidden)]
pub use core::ops::Shr as std_ops_Shr;
#[doc(hidden)]
pub use core::ops::Sub as std_ops_Sub;
#[doc(hidden)]
pub use generics::concat as generics_concat;
#[doc(hidden)]
pub use generics::parse as generics_parse;

#[doc(hidden)]
#[macro_export]
macro_rules! def_lt_a {
    (
        [] $callback:path { $($args:tt)* }
    ) => {
        $callback ! {
            ['newtype_derive_a] $($args)*
        }
    };
    (
        [$a:lifetime] $callback:path { $($args:tt)* }
    ) => {
        $callback ! {
            [$a] $($args)*
        }
    };
}

#[doc(hidden)]
#[macro_export]
macro_rules! def_lt_a_b {
    (
        [] [] $callback:path { $($args:tt)* }
    ) => {
        $callback ! {
            ['newtype_derive_a] ['newtype_derive_b] $($args)*
        }
    };
    (
        [$a:lifetime] [] $callback:path { $($args:tt)* }
    ) => {
        $callback ! {
            [$a] ['newtype_derive_b] $($args)*
        }
    };
    (
        [] [$b:lifetime] $callback:path { $($args:tt)* }
    ) => {
        $callback ! {
            ['newtype_derive_a] [$b] $($args)*
        }
    };
    (
        [$a:lifetime] [$b:lifetime] $callback:path { $($args:tt)* }
    ) => {
        $callback ! {
            [$a] [$b] $($args)*
        }
    };
}

#[doc(hidden)]
#[macro_export]
macro_rules! as_item {
    ($i:item) => {$i};
}

#[doc(hidden)]
#[macro_export]
macro_rules! wrap_bin_op {
    (
        trait: ($($tr:tt)*)::$meth:ident,
        kind: simple,
        item: [$name:ident] [$($bound:tt)*] [$($body:tt)+]
    ) => {
        $crate::generics_parse! {
            $crate::wrap_bin_op {
                generics_parse_done
                [
                    trait: ($($tr)*)::$meth,
                    kind: simple,
                    item: [$name] [$($bound)*]
                ]
            }
            $($body)+
        }
    };
    (
        generics_parse_done
        [
            trait: ($($tr:tt)*)::$meth:ident,
            kind: simple,
            item: [$name:ident] [$($bound:tt)*]
        ]
        [$($g:tt)*] [$($r:tt)*] [$($w:tt)*]
        ($(pub)? $t0:ty $(, $(pub)? $phantom:ty)* $(,)?);
    ) => {
        $crate::generics_concat! {
            $crate::wrap_bin_op {
                generics_concat_done
                [
                    trait: ($($tr)*)::$meth,
                    kind: simple,
                    item: [$name] [$t0] [$([$phantom])*]
                ]
            }
            [$($g)*] [$($r)*] [$($w)*],
            [] [] [where $($bound)*]
        }
    };
    (
        generics_concat_done
        [
            trait: ($($tr:tt)*)::$meth:ident,
            kind: simple,
            item: [$name:ident] [$t0:ty] [$([$phantom:ty])*]
        ]
        [$($g:tt)*] [$($r:tt)*] [$($w:tt)*]
    ) => {
        $crate::as_item! {
            impl $($g)* $($tr)*<$name $($r)*> for $name $($r)* $($w)* {
                type Output = Self;
                fn $meth(self, rhs: Self) -> Self {
                    $name(
                        <$t0 as $($tr)*<$t0>>::$meth(self.0, rhs.0)
                        $(, <$phantom as $crate::std_default_Default>::default())*
                    )
                }
            }
        }
    };

    (
        [$a:lifetime]
        trait: ($($tr:tt)*)::$meth:ident,
        kind: simple_ref,
        item: [$name:ident] [$($bound:tt)*] [$($body:tt)+]
    ) => {
        $crate::generics_parse! {
            $crate::wrap_bin_op {
                generics_parse_done
                [
                    trait: ($($tr)*)::$meth,
                    kind: simple_ref,
                    item: [$a] [$name] [$($bound)*]
                ]
            }
            $($body)+
        }
    };
    (
        generics_parse_done
        [
            trait: ($($tr:tt)*)::$meth:ident,
            kind: simple_ref,
            item: [$a:lifetime] [$name:ident] [$($bound:tt)*]
        ]
        [$($g:tt)*] [$($r:tt)*] [$($w:tt)*]
        ($(pub)? $t0:ty $(, $(pub)? $phantom:ty)* $(,)?);
    ) => {
        $crate::generics_concat! {
            $crate::wrap_bin_op {
                generics_concat_done
                [
                    trait: ($($tr)*)::$meth,
                    kind: simple_ref,
                    item: [$a] [$name] [$t0] [$([$phantom])*]
                ]
            }
            [ < $a > ] [] [],
            [$($g)*] [$($r)*] [$($w)*],
            [] [] [where $($bound)*]
        }
    };
    (
        generics_concat_done
        [
            trait: ($($tr:tt)*)::$meth:ident,
            kind: simple_ref,
            item: [$a:lifetime] [$name:ident] [$t0:ty] [$([$phantom:ty])*]
        ]
        [$($g:tt)*] [$($r:tt)*] [$($w:tt)*]
    ) => {
        $crate::as_item! {
            impl $($g)* $($tr)*<$name $($r)*> for & $a $name $($r)* $($w)* {
                type Output = $name $($r)*;
                fn $meth(self, rhs: $name $($r)*) -> $name $($r)* {
                    $name(
                        <$t0 as $($tr)*<$t0>>::$meth(self.0, rhs.0)
                        $(, <$phantom as $crate::std_default_Default>::default())*
                    )
                }
            }
        }
    };

    (
        trait: ($($tr:tt)*)::$meth:ident,
        kind: rhs_rewrap(&Self),
        item: [$a:lifetime] [$name:ident] [$($bound:tt)*] [$($body:tt)+]
    ) => {
        $crate::generics_parse! {
            $crate::wrap_bin_op {
                generics_parse_done
                [
                    trait: ($($tr)*)::$meth,
                    kind: rhs_rewrap(&Self),
                    item: [$a] [$name] [$($bound)*]
                ]
            }
            $($body)+
        }
    };
    (
        generics_parse_done
        [
            trait: ($($tr:tt)*)::$meth:ident,
            kind: rhs_rewrap(&Self),
            item: [$a:lifetime] [$name:ident] [$($bound:tt)*]
        ]
        [$($g:tt)*] [$($r:tt)*] [$($w:tt)*]
        ($(pub)? $t0:ty $(, $(pub)? $phantom:ty)* $(,)?);
    ) => {
        $crate::generics_concat! {
            $crate::wrap_bin_op {
                generics_concat_done
                [
                    trait: ($($tr)*)::$meth,
                    kind: rhs_rewrap(&Self),
                    item: [$a] [$name] [$t0] [$([$phantom])*]
                ]
            }
            [ < $a > ] [] [],
            [$($g)*] [$($r)*] [$($w)*],
            [] [] [where $($bound)*]
        }
    };
    (
        generics_concat_done
        [
            trait: ($($tr:tt)*)::$meth:ident,
            kind: rhs_rewrap(&Self),
            item: [$a:lifetime] [$name:ident] [$t0:ty] [$([$phantom:ty])*]
        ]
        [$($g:tt)*] [$($r:tt)*] [$($w:tt)*]
    ) => {
        $crate::as_item! {
            impl $($g)* $($tr)*<& $a $name $($r)*> for $name $($r)* $($w)* {
                type Output = Self;
                fn $meth(self, rhs: & $a Self) -> Self {
                    $name(
                        <$t0 as $($tr)*<$t0>>::$meth(self.0, rhs.0)
                        $(, <$phantom as $crate::std_default_Default>::default())*
                    )
                }
            }
        }
    };

    (
        trait: ($($tr:tt)*)::$meth:ident,
        kind: rhs_rewrap($Rhs:ty),
        item: [$name:ident] [$($lt:tt)*] [$($T:tt)*] [$($bound:tt)*] [$($body:tt)+]
    ) => {
        $crate::generics_parse! {
            $crate::wrap_bin_op {
                generics_parse_done
                [
                    trait: ($($tr)*)::$meth,
                    kind: rhs_rewrap($Rhs),
                    item: [$name] [$($lt)*] [$($T)*] [$($bound)*]
                ]
            }
            $($body)+
        }
    };
    (
        generics_parse_done
        [
            trait: ($($tr:tt)*)::$meth:ident,
            kind: rhs_rewrap($Rhs:ty),
            item: [$name:ident] [$($lt:tt)*] [$($T:tt)*] [$($bound:tt)*]
        ]
        [$($g:tt)*] [$($r:tt)*] [$($w:tt)*]
        ($(pub)? $t0:ty $(, $(pub)? $phantom:ty)* $(,)?);
    ) => {
        $crate::generics_concat! {
            $crate::wrap_bin_op {
                generics_concat_done
                [
                    trait: ($($tr)*)::$meth,
                    kind: rhs_rewrap($Rhs),
                    item: [$name] [$t0] [$([$phantom])*]
                ]
            }
            [$($lt)*] [] [],
            [$($g)*] [$($r)*] [$($w)*],
            [$($T)*] [] [],
            [] [] [where $($bound)*]
        }
    };
    (
        generics_concat_done
        [
            trait: ($($tr:tt)*)::$meth:ident,
            kind: rhs_rewrap($Rhs:ty),
            item: [$name:ident] [$t0:ty] [$([$phantom:ty])*]
        ]
        [$($g:tt)*] [$($r:tt)*] [$($w:tt)*]
    ) => {
        $crate::as_item! {
            impl $($g)* $($tr)*<$Rhs> for $name $($r)* $($w)* {
                type Output = Self;
                fn $meth(self, rhs: $Rhs) -> Self {
                    $name(
                        <$t0 as $($tr)*<$Rhs>>::$meth(self.0, rhs)
                        $(, <$phantom as $crate::std_default_Default>::default())*
                    )
                }
            }
        }
    };

    (
        [$a:lifetime] [$b:lifetime]
        trait: ($($tr:tt)*)::$meth:ident,
        kind: ref_rhs_rewrap(&Self),
        item: [$name:ident] [$($bound:tt)*] [$($body:tt)+]
    ) => {
        $crate::generics_parse! {
            $crate::wrap_bin_op {
                generics_parse_done
                [
                    trait: ($($tr)*)::$meth,
                    kind: ref_rhs_rewrap(&Self),
                    item: [$a] [$b] [$name] [$($bound)*]
                ]
            }
            $($body)+
        }
    };
    (
        generics_parse_done
        [
            trait: ($($tr:tt)*)::$meth:ident,
            kind: ref_rhs_rewrap(&Self),
            item: [$a:lifetime] [$b:lifetime] [$name:ident] [$($bound:tt)*]
        ]
        [$($g:tt)*] [$($r:tt)*] [$($w:tt)*]
        ($(pub)? $t0:ty $(, $(pub)? $phantom:ty)* $(,)?);
    ) => {
        $crate::generics_concat! {
            $crate::wrap_bin_op {
                generics_concat_done
                [
                    trait: ($($tr)*)::$meth,
                    kind: ref_rhs_rewrap(&Self),
                    item: [$a] [$b] [$name] [$t0] [$([$phantom])*]
                ]
            }
            [ < $a, $b > ] [] [],
            [$($g)*] [$($r)*] [$($w)*],
            [] [] [where $($bound)*]
        }
    };
    (
        generics_concat_done
        [
            trait: ($($tr:tt)*)::$meth:ident,
            kind: ref_rhs_rewrap(&Self),
            item: [$a:lifetime] [$b:lifetime] [$name:ident] [$t0:ty] [$([$phantom:ty])*]
        ]
        [$($g:tt)*] [$($r:tt)*] [$($w:tt)*]
    ) => {
        $crate::as_item! {
            impl $($g)* $($tr)*<& $b $name $($r)*> for & $a $name $($r)* $($w)* {
                type Output = $name $($r)*;
                fn $meth(self, rhs: & $b $name $($r)*) -> $name $($r)* {
                    $name(
                        <$t0 as $($tr)*<$t0>>::$meth(self.0, rhs.0)
                        $(, <$phantom as $crate::std_default_Default>::default())*
                    )
                }
            }
        }
    };

    (
        [$a:lifetime]
        trait: ($($tr:tt)*)::$meth:ident,
        kind: ref_rhs_rewrap($Rhs:ty),
        item: [$name:ident] [$($lt:tt)*] [$($T:tt)*] [$($bound:tt)*] [$($body:tt)+]
    ) => {
        $crate::generics_parse! {
            $crate::wrap_bin_op {
                generics_parse_done
                [
                    trait: ($($tr)*)::$meth,
                    kind: ref_rhs_rewrap($Rhs),
                    item: [$a] [$name] [$($lt)*] [$($T)*] [$($bound)*]
                ]
            }
            $($body)+
        }
    };
    (
        generics_parse_done
        [
            trait: ($($tr:tt)*)::$meth:ident,
            kind: ref_rhs_rewrap($Rhs:ty),
            item: [$a:lifetime] [$name:ident] [$($lt:tt)*] [$($T:tt)*] [$($bound:tt)*]
        ]
        [$($g:tt)*] [$($r:tt)*] [$($w:tt)*]
        ($(pub)? $t0:ty $(, $(pub)? $phantom:ty)* $(,)?);
    ) => {
        $crate::generics_concat! {
            $crate::wrap_bin_op {
                generics_concat_done
                [
                    trait: ($($tr)*)::$meth,
                    kind: ref_rhs_rewrap($Rhs),
                    item: [$a] [$name] [$t0] [$([$phantom])*]
                ]
            }
            [ < $a > ] [] [],
            [$($lt)*] [] [],
            [$($g)*] [$($r)*] [$($w)*],
            [$($T)*] [] [],
            [] [] [where $($bound)*]
        }
    };
    (
        generics_concat_done
        [
            trait: ($($tr:tt)*)::$meth:ident,
            kind: ref_rhs_rewrap($Rhs:ty),
            item: [$a:lifetime] [$name:ident] [$t0:ty] [$([$phantom:ty])*]
        ]
        [$($g:tt)*] [$($r:tt)*] [$($w:tt)*]
    ) => {
        $crate::as_item! {
            impl $($g)* $($tr)*<$Rhs> for & $a $name $($r)* $($w)* {
                type Output = $name $($r)*;
                fn $meth(self, rhs: $Rhs) -> $name $($r)* {
                    $name(
                        <$t0 as $($tr)*<$Rhs>>::$meth(self.0, rhs)
                        $(, <$phantom as $crate::std_default_Default>::default())*
                    )
                }
            }
        }
    };
}

#[macro_export]
macro_rules! NewtypeAdd {
    ((* $(where $($bound:tt)*)?) $vis:vis struct $name:ident $($body:tt)+) => {
        $crate::NewtypeAdd! { ($(where $($bound)*)?) $vis struct $name $($body)+ }
        $crate::NewtypeAdd! { (&self $(where $($bound)*)?) $vis struct $name $($body)+ }
        $crate::NewtypeAdd! { (&Self $(where $($bound)*)?) $vis struct $name $($body)+ }
        $crate::NewtypeAdd! { (&self, &Self $(where $($bound)*)?) $vis struct $name $($body)+ }
    };
    (($(where $($bound:tt)*)?) $vis:vis struct $name:ident $($body:tt)+) => {
        $crate::wrap_bin_op! { trait: ($crate::std_ops_Add)::add, kind: simple, item: [$name] [$($($bound)*)?] [$($body)+] }
    };
    ((& $($a:lifetime)? self $(where $($bound:tt)*)?) $vis:vis struct $name:ident $($body:tt)+) => {
        $crate::def_lt_a! {
            [$($a)?]
            $crate::wrap_bin_op { trait: ($crate::std_ops_Add)::add, kind: simple_ref, item: [$name] [$($($bound)*)?] [$($body)+] }
        }
    };
    ((& $($a:lifetime)? self, & $($b:lifetime)? Self $(where $($bound:tt)*)?) $vis:vis struct $name:ident $($body:tt)+) => {
        $crate::def_lt_a_b! {
            [$($a)?] [$($b)?]
            $crate::wrap_bin_op { trait: ($crate::std_ops_Add)::add, kind: ref_rhs_rewrap(&Self), item: [$name] [$($($bound)*)?] [$($body)+] }
        }
    };
    ((& $($a:lifetime)? self, $Rhs:ty $(where $($bound:tt)*)?) $vis:vis struct $name:ident $($body:tt)+) => {
        $crate::def_lt_a! {
            [$($a)?]
            $crate::wrap_bin_op { trait: ($crate::std_ops_Add)::add, kind: ref_rhs_rewrap($Rhs), item: [$name] [] [] [$($($bound)*)?] [$($body)+] }
        }
    };
    ((<$($lt:lifetime),+ $(, $($T:ident),+)? $(,)?> & $($a:lifetime)? self, $Rhs:ty $(where $($bound:tt)*)?) $vis:vis struct $name:ident $($body:tt)+) => {
        $crate::def_lt_a! {
            [$($a)?]
            $crate::wrap_bin_op { trait: ($crate::std_ops_Add)::add, kind: ref_rhs_rewrap($Rhs), item: [$name] [ < $($lt),+ > ] [ $( < $($T),+ > )? ] [$($($bound)*)?] [$($body)+] }
        }
    };
    ((<$($($T:ident),+ $(,)?)?> & $($a:lifetime)? self, $Rhs:ty $(where $($bound:tt)*)?) $vis:vis struct $name:ident $($body:tt)+) => {
        $crate::def_lt_a! {
            [$($a)?]
            $crate::wrap_bin_op { trait: ($crate::std_ops_Add)::add, kind: ref_rhs_rewrap($Rhs), item: [$name] [] [ $( < $($T),+ > )? ] [$($($bound)*)?] [$($body)+] }
        }
    };
    ((&Self $(where $($bound:tt)*)?) $vis:vis struct $name:ident $($body:tt)+) => {
        $crate::wrap_bin_op! { trait: ($crate::std_ops_Add)::add, kind: rhs_rewrap(&Self), item: ['newtype_derive] [$name] [$($($bound)*)?] [$($body)+] }
    };
    ((& $a:lifetime Self $(where $($bound:tt)*)?) $vis:vis struct $name:ident $($body:tt)+) => {
        $crate::wrap_bin_op! { trait: ($crate::std_ops_Add)::add, kind: rhs_rewrap(&Self), item: [$a] [$name] [$($($bound)*)?] [$($body)+] }
    };
    (($Rhs:ty $(where $($bound:tt)*)?) $vis:vis struct $name:ident $($body:tt)+) => {
        $crate::wrap_bin_op! { trait: ($crate::std_ops_Add)::add, kind: rhs_rewrap($Rhs), item: [$name] [] [] [$($($bound)*)?] [$($body)+] }
    };
    ((<$($lt:lifetime),+ $(, $($T:ident),+)? $(,)?> $Rhs:ty $(where $($bound:tt)*)?) $vis:vis struct $name:ident $($body:tt)+) => {
        $crate::wrap_bin_op! { trait: ($crate::std_ops_Add)::add, kind: rhs_rewrap($Rhs), item: [$name] [ < $($lt),+ > ] [ $( < $($T),+ > )? ] [$($($bound)*)?] [$($body)+] }
    };
    ((<$($($T:ident),+ $(,)?)?> $Rhs:ty $(where $($bound:tt)*)?) $vis:vis struct $name:ident $($body:tt)+) => {
        $crate::wrap_bin_op! { trait: ($crate::std_ops_Add)::add, kind: rhs_rewrap($Rhs), item: [$name] [] [ $( < $($T),+ > )? ] [$($($bound)*)?] [$($body)+] }
    };
}

#[macro_export]
macro_rules! NewtypeBitAnd {
    ((* $(where $($bound:tt)*)?) $vis:vis struct $name:ident $($body:tt)+) => {
        $crate::NewtypeBitAnd! { ($(where $($bound)*)?) $vis struct $name $($body)+ }
        $crate::NewtypeBitAnd! { (&self $(where $($bound)*)?) $vis struct $name $($body)+ }
        $crate::NewtypeBitAnd! { (&Self $(where $($bound)*)?) $vis struct $name $($body)+ }
        $crate::NewtypeBitAnd! { (&self, &Self $(where $($bound)*)?) $vis struct $name $($body)+ }
    };
    (($(where $($bound:tt)*)?) $vis:vis struct $name:ident $($body:tt)+) => {
        $crate::wrap_bin_op! { trait: ($crate::std_ops_BitAnd)::bitand, kind: simple, item: [$name] [$($($bound)*)?] [$($body)+] }
    };
    ((&self $(where $($bound:tt)*)?) $vis:vis struct $name:ident $($body:tt)+) => {
        $crate::wrap_bin_op! { ['newtype_derive] trait: ($crate::std_ops_BitAnd)::bitand, kind: simple_ref, item: [$name] [$($($bound)*)?] [$($body)+] }
    };
    ((& $($a:lifetime)? self $(where $($bound:tt)*)?) $vis:vis struct $name:ident $($body:tt)+) => {
        $crate::def_lt_a! {
            [$($a)?]
            $crate::wrap_bin_op { trait: ($crate::std_ops_BitAnd)::bitand, kind: simple_ref, item: [$name] [$($($bound)*)?] [$($body)+] }
        }
    };
    ((& $($a:lifetime)? self, & $($b:lifetime)? Self $(where $($bound:tt)*)?) $vis:vis struct $name:ident $($body:tt)+) => {
        $crate::def_lt_a_b! {
            [$($a)?] [$($b)?]
            $crate::wrap_bin_op { trait: ($crate::std_ops_BitAnd)::bitand, kind: ref_rhs_rewrap(&Self), item: [$name] [$($($bound)*)?] [$($body)+] }
        }
    };
    ((& $($a:lifetime)? self, $Rhs:ty $(where $($bound:tt)*)?) $vis:vis struct $name:ident $($body:tt)+) => {
        $crate::def_lt_a! {
            [$($a)?]
            $crate::wrap_bin_op { trait: ($crate::std_ops_BitAnd)::bitand, kind: ref_rhs_rewrap($Rhs), item: [$name] [] [] [$($($bound)*)?] [$($body)+] }
        }
    };
    ((<$($lt:lifetime),+ $(, $($T:ident),+)? $(,)?> & $($a:lifetime)? self, $Rhs:ty $(where $($bound:tt)*)?) $vis:vis struct $name:ident $($body:tt)+) => {
        $crate::def_lt_a! {
            [$($a)?]
            $crate::wrap_bin_op { trait: ($crate::std_ops_BitAnd)::bitand, kind: ref_rhs_rewrap($Rhs), item: [$name] [ < $($lt),+ > ] [ $( < $($T),+ > )? ] [$($($bound)*)?] [$($body)+] }
        }
    };
    ((<$($($T:ident),+ $(,)?)?> & $a:lifetime self, $Rhs:ty $(where $($bound:tt)*)?) $vis:vis struct $name:ident $($body:tt)+) => {
        $crate::def_lt_a! {
            [$($a)?]
            $crate::wrap_bin_op { trait: ($crate::std_ops_BitAnd)::bitand, kind: ref_rhs_rewrap($Rhs), item: [$name] [] [ $( < $($T),+ > )? ] [$($($bound)*)?] [$($body)+] }
        }
    };
    ((&Self $(where $($bound:tt)*)?) $vis:vis struct $name:ident $($body:tt)+) => {
        $crate::wrap_bin_op! { trait: ($crate::std_ops_BitAnd)::bitand, kind: rhs_rewrap(&Self), item: ['newtype_derive] [$name] [$($($bound)*)?] [$($body)+] }
    };
    ((& $a:lifetime Self $(where $($bound:tt)*)?) $vis:vis struct $name:ident $($body:tt)+) => {
        $crate::wrap_bin_op! { trait: ($crate::std_ops_BitAnd)::bitand, kind: rhs_rewrap(&Self), item: [$a] [$name] [$($($bound)*)?] [$($body)+] }
    };
    (($Rhs:ty $(where $($bound:tt)*)?) $vis:vis struct $name:ident $($body:tt)+) => {
        $crate::wrap_bin_op! { trait: ($crate::std_ops_BitAnd)::bitand, kind: rhs_rewrap($Rhs), item: [$name] [] [] [$($($bound)*)?] [$($body)+] }
    };
    ((<$($lt:lifetime),+ $(, $($T:ident),+)? $(,)?> $Rhs:ty $(where $($bound:tt)*)?) $vis:vis struct $name:ident $($body:tt)+) => {
        $crate::wrap_bin_op! { trait: ($crate::std_ops_BitAnd)::bitand, kind: rhs_rewrap($Rhs), item: [$name] [ < $($lt),+ > ] [ $( < $($T),+ > )? ] [$($($bound)*)?] [$($body)+] }
    };
    ((<$($($T:ident),+ $(,)?)?> $Rhs:ty $(where $($bound:tt)*)?) $vis:vis struct $name:ident $($body:tt)+) => {
        $crate::wrap_bin_op! { trait: ($crate::std_ops_BitAnd)::bitand, kind: rhs_rewrap($Rhs), item: [$name] [] [ $( < $($T),+ > )? ] [$($($bound)*)?] [$($body)+] }
    };
}

#[macro_export]
macro_rules! NewtypeBitOr {
    ((* $(where $($bound:tt)*)?) $vis:vis struct $name:ident $($body:tt)+) => {
        $crate::NewtypeBitOr! { ($(where $($bound)*)?) $vis struct $name $($body)+ }
        $crate::NewtypeBitOr! { (&self $(where $($bound)*)?) $vis struct $name $($body)+ }
        $crate::NewtypeBitOr! { (&Self $(where $($bound)*)?) $vis struct $name $($body)+ }
        $crate::NewtypeBitOr! { (&self, &Self $(where $($bound)*)?) $vis struct $name $($body)+ }
    };
    (($(where $($bound:tt)*)?) $vis:vis struct $name:ident $($body:tt)+) => {
        $crate::wrap_bin_op! { trait: ($crate::std_ops_BitOr)::bitor, kind: simple, item: [$name] [$($($bound)*)?] [$($body)+] }
    };
    ((&self $(where $($bound:tt)*)?) $vis:vis struct $name:ident $($body:tt)+) => {
        $crate::wrap_bin_op! { ['newtype_derive] trait: ($crate::std_ops_BitOr)::bitor, kind: simple_ref, item: [$name] [$($($bound)*)?] [$($body)+] }
    };
    ((& $($a:lifetime)? self $(where $($bound:tt)*)?) $vis:vis struct $name:ident $($body:tt)+) => {
        $crate::def_lt_a! {
            [$($a)?]
            $crate::wrap_bin_op { trait: ($crate::std_ops_BitOr)::bitor, kind: simple_ref, item: [$name] [$($($bound)*)?] [$($body)+] }
        }
    };
    ((& $($a:lifetime)? self, & $($b:lifetime)? Self $(where $($bound:tt)*)?) $vis:vis struct $name:ident $($body:tt)+) => {
        $crate::def_lt_a_b! {
            [$($a)?] [$($b)?]
            $crate::wrap_bin_op { trait: ($crate::std_ops_BitOr)::bitor, kind: ref_rhs_rewrap(&Self), item: [$name] [$($($bound)*)?] [$($body)+] }
        }
    };
    ((& $($a:lifetime)? self, $Rhs:ty $(where $($bound:tt)*)?) $vis:vis struct $name:ident $($body:tt)+) => {
        $crate::def_lt_a! {
            [$($a)?]
            $crate::wrap_bin_op { trait: ($crate::std_ops_BitOr)::bitor, kind: ref_rhs_rewrap($Rhs), item: [$name] [] [] [$($($bound)*)?] [$($body)+] }
        }
    };
    ((<$($lt:lifetime),+ $(, $($T:ident),+)? $(,)?> & $($a:lifetime)? self, $Rhs:ty $(where $($bound:tt)*)?) $vis:vis struct $name:ident $($body:tt)+) => {
        $crate::def_lt_a! {
            [$($a)?]
            $crate::wrap_bin_op { trait: ($crate::std_ops_BitOr)::bitor, kind: ref_rhs_rewrap($Rhs), item: [$name] [ < $($lt),+ > ] [ $( < $($T),+ > )? ] [$($($bound)*)?] [$($body)+] }
        }
    };
    ((<$($($T:ident),+ $(,)?)?> & $($a:lifetime)? self, $Rhs:ty $(where $($bound:tt)*)?) $vis:vis struct $name:ident $($body:tt)+) => {
        $crate::def_lt_a! {
            [$($a)?]
            $crate::wrap_bin_op { trait: ($crate::std_ops_BitOr)::bitor, kind: ref_rhs_rewrap($Rhs), item: [$name] [] [ $( < $($T),+ > )? ] [$($($bound)*)?] [$($body)+] }
        }
    };
    ((&Self $(where $($bound:tt)*)?) $vis:vis struct $name:ident $($body:tt)+) => {
        $crate::wrap_bin_op! { trait: ($crate::std_ops_BitOr)::bitor, kind: rhs_rewrap(&Self), item: ['newtype_derive] [$name] [$($($bound)*)?] [$($body)+] }
    };
    ((& $a:lifetime Self $(where $($bound:tt)*)?) $vis:vis struct $name:ident $($body:tt)+) => {
        $crate::wrap_bin_op! { trait: ($crate::std_ops_BitOr)::bitor, kind: rhs_rewrap(&Self), item: [$a] [$name] [$($($bound)*)?] [$($body)+] }
    };
    (($Rhs:ty $(where $($bound:tt)*)?) $vis:vis struct $name:ident $($body:tt)+) => {
        $crate::wrap_bin_op! { trait: ($crate::std_ops_BitOr)::bitor, kind: rhs_rewrap($Rhs), item: [$name] [] [] [$($($bound)*)?] [$($body)+] }
    };
    ((<$($lt:lifetime),+ $(, $($T:ident),+)? $(,)?> $Rhs:ty $(where $($bound:tt)*)?) $vis:vis struct $name:ident $($body:tt)+) => {
        $crate::wrap_bin_op! { trait: ($crate::std_ops_BitOr)::bitor, kind: rhs_rewrap($Rhs), item: [$name] [ < $($lt),+ > ] [ $( < $($T),+ > )? ] [$($($bound)*)?] [$($body)+] }
    };
    ((<$($($T:ident),+ $(,)?)?> $Rhs:ty $(where $($bound:tt)*)?) $vis:vis struct $name:ident $($body:tt)+) => {
        $crate::wrap_bin_op! { trait: ($crate::std_ops_BitOr)::bitor, kind: rhs_rewrap($Rhs), item: [$name] [] [ $( < $($T),+ > )? ] [$($($bound)*)?] [$($body)+] }
    };
}

#[macro_export]
macro_rules! NewtypeBitXor {
    ((* $(where $($bound:tt)*)?) $vis:vis struct $name:ident $($body:tt)+) => {
        $crate::NewtypeBitXor! { ($(where $($bound)*)?) $vis struct $name $($body)+ }
        $crate::NewtypeBitXor! { (&self $(where $($bound)*)?) $vis struct $name $($body)+ }
        $crate::NewtypeBitXor! { (&Self $(where $($bound)*)?) $vis struct $name $($body)+ }
        $crate::NewtypeBitXor! { (&self, &Self $(where $($bound)*)?) $vis struct $name $($body)+ }
    };
    (($(where $($bound:tt)*)?) $vis:vis struct $name:ident $($body:tt)+) => {
        $crate::wrap_bin_op! { trait: ($crate::std_ops_BitXor)::bitxor, kind: simple, item: [$name] [$($($bound)*)?] [$($body)+] }
    };
    ((&self $(where $($bound:tt)*)?) $vis:vis struct $name:ident $($body:tt)+) => {
        $crate::wrap_bin_op! { ['newtype_derive] trait: ($crate::std_ops_BitXor)::bitxor, kind: simple_ref, item: [$name] [$($($bound)*)?] [$($body)+] }
    };
    ((& $($a:lifetime)? self $(where $($bound:tt)*)?) $vis:vis struct $name:ident $($body:tt)+) => {
        $crate::def_lt_a! {
            [$($a)?]
            $crate::wrap_bin_op { trait: ($crate::std_ops_BitXor)::bitxor, kind: simple_ref, item: [$name] [$($($bound)*)?] [$($body)+] }
        }
    };
    ((& $($a:lifetime)? self, & $($b:lifetime)? Self $(where $($bound:tt)*)?) $vis:vis struct $name:ident $($body:tt)+) => {
        $crate::def_lt_a_b! {
            [$($a)?] [$($b)?]
            $crate::wrap_bin_op { trait: ($crate::std_ops_BitXor)::bitxor, kind: ref_rhs_rewrap(&Self), item: [$name] [$($($bound)*)?] [$($body)+] }
        }
    };
    ((& $($a:lifetime)? self, $Rhs:ty $(where $($bound:tt)*)?) $vis:vis struct $name:ident $($body:tt)+) => {
        $crate::def_lt_a! {
            [$($a)?]
            $crate::wrap_bin_op { trait: ($crate::std_ops_BitXor)::bitxor, kind: ref_rhs_rewrap($Rhs), item: [$name] [] [] [$($($bound)*)?] [$($body)+] }
        }
    };
    ((<$($lt:lifetime),+ $(, $($T:ident),+)? $(,)?> & $($a:lifetime)? self, $Rhs:ty $(where $($bound:tt)*)?) $vis:vis struct $name:ident $($body:tt)+) => {
        $crate::def_lt_a! {
            [$($a)?]
            $crate::wrap_bin_op { trait: ($crate::std_ops_BitXor)::bitxor, kind: ref_rhs_rewrap($Rhs), item: [$name] [ < $($lt),+ > ] [ $( < $($T),+ > )? ] [$($($bound)*)?] [$($body)+] }
        }
    };
    ((<$($($T:ident),+ $(,)?)?> & $($a:lifetime)? self, $Rhs:ty $(where $($bound:tt)*)?) $vis:vis struct $name:ident $($body:tt)+) => {
        $crate::def_lt_a! {
            [$($a)?]
            $crate::wrap_bin_op { trait: ($crate::std_ops_BitXor)::bitxor, kind: ref_rhs_rewrap($Rhs), item: [$name] [] [ $( < $($T),+ > )? ] [$($($bound)*)?] [$($body)+] }
        }
    };
    ((&Self $(where $($bound:tt)*)?) $vis:vis struct $name:ident $($body:tt)+) => {
        $crate::wrap_bin_op! { trait: ($crate::std_ops_BitXor)::bitxor, kind: rhs_rewrap(&Self), item: ['newtype_derive] [$name] [$($($bound)*)?] [$($body)+] }
    };
    ((& $a:lifetime Self $(where $($bound:tt)*)?) $vis:vis struct $name:ident $($body:tt)+) => {
        $crate::wrap_bin_op! { trait: ($crate::std_ops_BitXor)::bitxor, kind: rhs_rewrap(&Self), item: [$a] [$name] [$($($bound)*)?] [$($body)+] }
    };
    (($Rhs:ty $(where $($bound:tt)*)?) $vis:vis struct $name:ident $($body:tt)+) => {
        $crate::wrap_bin_op! { trait: ($crate::std_ops_BitXor)::bitxor, kind: rhs_rewrap($Rhs), item: [$name] [] [] [$($($bound)*)?] [$($body)+] }
    };
    ((<$($lt:lifetime),+ $(, $($T:ident),+)? $(,)?> $Rhs:ty $(where $($bound:tt)*)?) $vis:vis struct $name:ident $($body:tt)+) => {
        $crate::wrap_bin_op! { trait: ($crate::std_ops_BitXor)::bitxor, kind: rhs_rewrap($Rhs), item: [$name] [ < $($lt),+ > ] [ $( < $($T),+ > )? ] [$($($bound)*)?] [$($body)+] }
    };
    ((<$($($T:ident),+ $(,)?)?> $Rhs:ty $(where $($bound:tt)*)?) $vis:vis struct $name:ident $($body:tt)+) => {
        $crate::wrap_bin_op! { trait: ($crate::std_ops_BitXor)::bitxor, kind: rhs_rewrap($Rhs), item: [$name] [] [ $( < $($T),+ > )? ] [$($($bound)*)?] [$($body)+] }
    };
}

#[macro_export]
macro_rules! NewtypeDiv {
    ((* $(where $($bound:tt)*)?) $vis:vis struct $name:ident $($body:tt)+) => {
        $crate::NewtypeDiv! { ($(where $($bound)*)?) $vis struct $name $($body)+ }
        $crate::NewtypeDiv! { (&self $(where $($bound)*)?) $vis struct $name $($body)+ }
        $crate::NewtypeDiv! { (&Self $(where $($bound)*)?) $vis struct $name $($body)+ }
        $crate::NewtypeDiv! { (&self, &Self $(where $($bound)*)?) $vis struct $name $($body)+ }
    };
    (($(where $($bound:tt)*)?) $vis:vis struct $name:ident $($body:tt)+) => {
        $crate::wrap_bin_op! { trait: ($crate::std_ops_Div)::div, kind: simple, item: [$name] [$($($bound)*)?] [$($body)+] }
    };
    ((&self $(where $($bound:tt)*)?) $vis:vis struct $name:ident $($body:tt)+) => {
        $crate::wrap_bin_op! { ['newtype_derive] trait: ($crate::std_ops_Div)::div, kind: simple_ref, item: [$name] [$($($bound)*)?] [$($body)+] }
    };
    ((& $($a:lifetime)? self $(where $($bound:tt)*)?) $vis:vis struct $name:ident $($body:tt)+) => {
        $crate::def_lt_a! {
            [$($a)?]
            $crate::wrap_bin_op { trait: ($crate::std_ops_Div)::div, kind: simple_ref, item: [$name] [$($($bound)*)?] [$($body)+] }
        }
    };
    ((& $($a:lifetime)? self, & $($b:lifetime)? Self $(where $($bound:tt)*)?) $vis:vis struct $name:ident $($body:tt)+) => {
        $crate::def_lt_a_b! {
            [$($a)?] [$($b)?]
            $crate::wrap_bin_op { trait: ($crate::std_ops_Div)::div, kind: ref_rhs_rewrap(&Self), item: [$name] [$($($bound)*)?] [$($body)+] }
        }
    };
    ((& $($a:lifetime)? self, $Rhs:ty $(where $($bound:tt)*)?) $vis:vis struct $name:ident $($body:tt)+) => {
        $crate::def_lt_a! {
            [$($a)?]
            $crate::wrap_bin_op { trait: ($crate::std_ops_Div)::div, kind: ref_rhs_rewrap($Rhs), item: [$name] [] [] [$($($bound)*)?] [$($body)+] }
        }
    };
    ((<$($lt:lifetime),+ $(, $($T:ident),+)? $(,)?> & $($a:lifetime)? self, $Rhs:ty $(where $($bound:tt)*)?) $vis:vis struct $name:ident $($body:tt)+) => {
        $crate::def_lt_a! {
            [$($a)?]
            $crate::wrap_bin_op { trait: ($crate::std_ops_Div)::div, kind: ref_rhs_rewrap($Rhs), item: [$name] [ < $($lt),+ > ] [ $( < $($T),+ > )? ] [$($($bound)*)?] [$($body)+] }
        }
    };
    ((<$($($T:ident),+ $(,)?)?> & $($a:lifetime)? self, $Rhs:ty $(where $($bound:tt)*)?) $vis:vis struct $name:ident $($body:tt)+) => {
        $crate::def_lt_a! {
            [$($a)?]
            $crate::wrap_bin_op { trait: ($crate::std_ops_Div)::div, kind: ref_rhs_rewrap($Rhs), item: [$name] [] [ $( < $($T),+ > )? ] [$($($bound)*)?] [$($body)+] }
        }
    };
    ((&Self $(where $($bound:tt)*)?) $vis:vis struct $name:ident $($body:tt)+) => {
        $crate::wrap_bin_op! { trait: ($crate::std_ops_Div)::div, kind: rhs_rewrap(&Self), item: ['newtype_derive] [$name] [$($($bound)*)?] [$($body)+] }
    };
    ((& $a:lifetime Self $(where $($bound:tt)*)?) $vis:vis struct $name:ident $($body:tt)+) => {
        $crate::wrap_bin_op! { trait: ($crate::std_ops_Div)::div, kind: rhs_rewrap(&Self), item: [$a] [$name] [$($($bound)*)?] [$($body)+] }
    };
    (($Rhs:ty $(where $($bound:tt)*)?) $vis:vis struct $name:ident $($body:tt)+) => {
        $crate::wrap_bin_op! { trait: ($crate::std_ops_Div)::div, kind: rhs_rewrap($Rhs), item: [$name] [] [] [$($($bound)*)?] [$($body)+] }
    };
    ((<$($lt:lifetime),+ $(, $($T:ident),+)? $(,)?> $Rhs:ty $(where $($bound:tt)*)?) $vis:vis struct $name:ident $($body:tt)+) => {
        $crate::wrap_bin_op! { trait: ($crate::std_ops_Div)::div, kind: rhs_rewrap($Rhs), item: [$name] [ < $($lt),+ > ] [ $( < $($T),+ > )? ] [$($($bound)*)?] [$($body)+] }
    };
    ((<$($($T:ident),+ $(,)?)?> $Rhs:ty $(where $($bound:tt)*)?) $vis:vis struct $name:ident $($body:tt)+) => {
        $crate::wrap_bin_op! { trait: ($crate::std_ops_Div)::div, kind: rhs_rewrap($Rhs), item: [$name] [] [ $( < $($T),+ > )? ] [$($($bound)*)?] [$($body)+] }
    };
}

#[macro_export]
macro_rules! NewtypeMul {
    ((* $(where $($bound:tt)*)?) $vis:vis struct $name:ident $($body:tt)+) => {
        $crate::NewtypeMul! { ($(where $($bound)*)?) $vis struct $name $($body)+ }
        $crate::NewtypeMul! { (&self $(where $($bound)*)?) $vis struct $name $($body)+ }
        $crate::NewtypeMul! { (&Self $(where $($bound)*)?) $vis struct $name $($body)+ }
        $crate::NewtypeMul! { (&self, &Self $(where $($bound)*)?) $vis struct $name $($body)+ }
    };
    (($(where $($bound:tt)*)?) $vis:vis struct $name:ident $($body:tt)+) => {
        $crate::wrap_bin_op! { trait: ($crate::std_ops_Mul)::mul, kind: simple, item: [$name] [$($($bound)*)?] [$($body)+] }
    };
    ((&self $(where $($bound:tt)*)?) $vis:vis struct $name:ident $($body:tt)+) => {
        $crate::wrap_bin_op! { ['newtype_derive] trait: ($crate::std_ops_Mul)::mul, kind: simple_ref, item: [$name] [$($($bound)*)?] [$($body)+] }
    };
    ((& $($a:lifetime)? self $(where $($bound:tt)*)?) $vis:vis struct $name:ident $($body:tt)+) => {
        $crate::def_lt_a! {
            [$($a)?]
            $crate::wrap_bin_op { trait: ($crate::std_ops_Mul)::mul, kind: simple_ref, item: [$name] [$($($bound)*)?] [$($body)+] }
        }
    };
    ((& $($a:lifetime)? self, & $($b:lifetime)? Self $(where $($bound:tt)*)?) $vis:vis struct $name:ident $($body:tt)+) => {
        $crate::def_lt_a_b! {
            [$($a)?] [$($b)?]
            $crate::wrap_bin_op { trait: ($crate::std_ops_Mul)::mul, kind: ref_rhs_rewrap(&Self), item: [$name] [$($($bound)*)?] [$($body)+] }
        }
    };
    ((& $($a:lifetime)? self, $Rhs:ty $(where $($bound:tt)*)?) $vis:vis struct $name:ident $($body:tt)+) => {
        $crate::def_lt_a! {
            [$($a)?]
            $crate::wrap_bin_op { trait: ($crate::std_ops_Mul)::mul, kind: ref_rhs_rewrap($Rhs), item: [$name] [] [] [$($($bound)*)?] [$($body)+] }
        }
    };
    ((<$($lt:lifetime),+ $(, $($T:ident),+)? $(,)?> & $($a:lifetime)? self, $Rhs:ty $(where $($bound:tt)*)?) $vis:vis struct $name:ident $($body:tt)+) => {
        $crate::def_lt_a! {
            [$($a)?]
            $crate::wrap_bin_op { trait: ($crate::std_ops_Mul)::mul, kind: ref_rhs_rewrap($Rhs), item: [$name] [ < $($lt),+ > ] [ $( < $($T),+ > )? ] [$($($bound)*)?] [$($body)+] }
        }
    };
    ((<$($($T:ident),+ $(,)?)?> & $($a:lifetime)? self, $Rhs:ty $(where $($bound:tt)*)?) $vis:vis struct $name:ident $($body:tt)+) => {
        $crate::def_lt_a! {
            [$($a)?]
            $crate::wrap_bin_op { trait: ($crate::std_ops_Mul)::mul, kind: ref_rhs_rewrap($Rhs), item: [$name] [] [ $( < $($T),+ > )? ] [$($($bound)*)?] [$($body)+] }
        }
    };
    ((&Self $(where $($bound:tt)*)?) $vis:vis struct $name:ident $($body:tt)+) => {
        $crate::wrap_bin_op! { trait: ($crate::std_ops_Mul)::mul, kind: rhs_rewrap(&Self), item: ['newtype_derive] [$name] [$($($bound)*)?] [$($body)+] }
    };
    ((& $a:lifetime Self $(where $($bound:tt)*)?) $vis:vis struct $name:ident $($body:tt)+) => {
        $crate::wrap_bin_op! { trait: ($crate::std_ops_Mul)::mul, kind: rhs_rewrap(&Self), item: [$a] [$name] [$($($bound)*)?] [$($body)+] }
    };
    (($Rhs:ty $(where $($bound:tt)*)?) $vis:vis struct $name:ident $($body:tt)+) => {
        $crate::wrap_bin_op! { trait: ($crate::std_ops_Mul)::mul, kind: rhs_rewrap($Rhs), item: [$name] [] [] [$($($bound)*)?] [$($body)+] }
    };
    ((<$($lt:lifetime),+ $(, $($T:ident),+)? $(,)?> $Rhs:ty $(where $($bound:tt)*)?) $vis:vis struct $name:ident $($body:tt)+) => {
        $crate::wrap_bin_op! { trait: ($crate::std_ops_Mul)::mul, kind: rhs_rewrap($Rhs), item: [$name] [ < $($lt),+ > ] [ $( < $($T),+ > )? ] [$($($bound)*)?] [$($body)+] }
    };
    ((<$($($T:ident),+ $(,)?)?> $Rhs:ty $(where $($bound:tt)*)?) $vis:vis struct $name:ident $($body:tt)+) => {
        $crate::wrap_bin_op! { trait: ($crate::std_ops_Mul)::mul, kind: rhs_rewrap($Rhs), item: [$name] [] [ $( < $($T),+ > )? ] [$($($bound)*)?] [$($body)+] }
    };
}

#[macro_export]
macro_rules! NewtypeRem {
    ((* $(where $($bound:tt)*)?) $vis:vis struct $name:ident $($body:tt)+) => {
        $crate::NewtypeRem! { ($(where $($bound)*)?) $vis struct $name $($body)+ }
        $crate::NewtypeRem! { (&self $(where $($bound)*)?) $vis struct $name $($body)+ }
        $crate::NewtypeRem! { (&Self $(where $($bound)*)?) $vis struct $name $($body)+ }
        $crate::NewtypeRem! { (&self, &Self $(where $($bound)*)?) $vis struct $name $($body)+ }
    };
    (($(where $($bound:tt)*)?) $vis:vis struct $name:ident $($body:tt)+) => {
        $crate::wrap_bin_op! { trait: ($crate::std_ops_Rem)::rem, kind: simple, item: [$name] [$($($bound)*)?] [$($body)+] }
    };
    ((&self $(where $($bound:tt)*)?) $vis:vis struct $name:ident $($body:tt)+) => {
        $crate::wrap_bin_op! { ['newtype_derive] trait: ($crate::std_ops_Rem)::rem, kind: simple_ref, item: [$name] [$($($bound)*)?] [$($body)+] }
    };
    ((& $($a:lifetime)? self $(where $($bound:tt)*)?) $vis:vis struct $name:ident $($body:tt)+) => {
        $crate::def_lt_a! {
            [$($a)?]
            $crate::wrap_bin_op { trait: ($crate::std_ops_Rem)::rem, kind: simple_ref, item: [$name] [$($($bound)*)?] [$($body)+] }
        }
    };
    ((& $($a:lifetime)? self, & $($b:lifetime)? Self $(where $($bound:tt)*)?) $vis:vis struct $name:ident $($body:tt)+) => {
        $crate::def_lt_a_b! {
            [$($a)?] [$($b)?]
            $crate::wrap_bin_op { trait: ($crate::std_ops_Rem)::rem, kind: ref_rhs_rewrap(&Self), item: [$name] [$($($bound)*)?] [$($body)+] }
        }
    };
    ((& $($a:lifetime)? self, $Rhs:ty $(where $($bound:tt)*)?) $vis:vis struct $name:ident $($body:tt)+) => {
        $crate::def_lt_a! {
            [$($a)?]
            $crate::wrap_bin_op { trait: ($crate::std_ops_Rem)::rem, kind: ref_rhs_rewrap($Rhs), item: [$name] [] [] [$($($bound)*)?] [$($body)+] }
        }
    };
    ((<$($lt:lifetime),+ $(, $($T:ident),+)? $(,)?> & $($a:lifetime)? self, $Rhs:ty $(where $($bound:tt)*)?) $vis:vis struct $name:ident $($body:tt)+) => {
        $crate::def_lt_a! {
            [$($a)?]
            $crate::wrap_bin_op { trait: ($crate::std_ops_Rem)::rem, kind: ref_rhs_rewrap($Rhs), item: [$name] [ < $($lt),+ > ] [ $( < $($T),+ > )? ] [$($($bound)*)?] [$($body)+] }
        }
    };
    ((<$($($T:ident),+ $(,)?)?> & $($a:lifetime)? self, $Rhs:ty $(where $($bound:tt)*)?) $vis:vis struct $name:ident $($body:tt)+) => {
        $crate::def_lt_a! {
            [$($a)?]
            $crate::wrap_bin_op { trait: ($crate::std_ops_Rem)::rem, kind: ref_rhs_rewrap($Rhs), item: [$name] [] [ $( < $($T),+ > )? ] [$($($bound)*)?] [$($body)+] }
        }
    };
    ((&Self $(where $($bound:tt)*)?) $vis:vis struct $name:ident $($body:tt)+) => {
        $crate::wrap_bin_op! { trait: ($crate::std_ops_Rem)::rem, kind: rhs_rewrap(&Self), item: ['newtype_derive] [$name] [$($($bound)*)?] [$($body)+] }
    };
    ((& $a:lifetime Self $(where $($bound:tt)*)?) $vis:vis struct $name:ident $($body:tt)+) => {
        $crate::wrap_bin_op! { trait: ($crate::std_ops_Rem)::rem, kind: rhs_rewrap(&Self), item: [$a] [$name] [$($($bound)*)?] [$($body)+] }
    };
    (($Rhs:ty $(where $($bound:tt)*)?) $vis:vis struct $name:ident $($body:tt)+) => {
        $crate::wrap_bin_op! { trait: ($crate::std_ops_Rem)::rem, kind: rhs_rewrap($Rhs), item: [$name] [] [] [$($($bound)*)?] [$($body)+] }
    };
    ((<$($lt:lifetime),+ $(, $($T:ident),+)? $(,)?> $Rhs:ty $(where $($bound:tt)*)?) $vis:vis struct $name:ident $($body:tt)+) => {
        $crate::wrap_bin_op! { trait: ($crate::std_ops_Rem)::rem, kind: rhs_rewrap($Rhs), item: [$name] [ < $($lt),+ > ] [ $( < $($T),+ > )? ] [$($($bound)*)?] [$($body)+] }
    };
    ((<$($($T:ident),+ $(,)?)?> $Rhs:ty $(where $($bound:tt)*)?) $vis:vis struct $name:ident $($body:tt)+) => {
        $crate::wrap_bin_op! { trait: ($crate::std_ops_Rem)::rem, kind: rhs_rewrap($Rhs), item: [$name] [] [ $( < $($T),+ > )? ] [$($($bound)*)?] [$($body)+] }
    };
}

#[macro_export]
macro_rules! NewtypeSub {
    ((* $(where $($bound:tt)*)?) $vis:vis struct $name:ident $($body:tt)+) => {
        $crate::NewtypeSub! { ($(where $($bound)*)?) $vis struct $name $($body)+ }
        $crate::NewtypeSub! { (&self $(where $($bound)*)?) $vis struct $name $($body)+ }
        $crate::NewtypeSub! { (&Self $(where $($bound)*)?) $vis struct $name $($body)+ }
        $crate::NewtypeSub! { (&self, &Self $(where $($bound)*)?) $vis struct $name $($body)+ }
    };
    (($(where $($bound:tt)*)?) $vis:vis struct $name:ident $($body:tt)+) => {
        $crate::wrap_bin_op! { trait: ($crate::std_ops_Sub)::sub, kind: simple, item: [$name] [$($($bound)*)?] [$($body)+] }
    };
    ((&self $(where $($bound:tt)*)?) $vis:vis struct $name:ident $($body:tt)+) => {
        $crate::wrap_bin_op! { ['newtype_derive] trait: ($crate::std_ops_Sub)::sub, kind: simple_ref, item: [$name] [$($($bound)*)?] [$($body)+] }
    };
    ((& $($a:lifetime)? self $(where $($bound:tt)*)?) $vis:vis struct $name:ident $($body:tt)+) => {
        $crate::def_lt_a! {
            [$($a)?]
            $crate::wrap_bin_op { trait: ($crate::std_ops_Sub)::sub, kind: simple_ref, item: [$name] [$($($bound)*)?] [$($body)+] }
        }
    };
    ((& $($a:lifetime)? self, & $($b:lifetime)? Self $(where $($bound:tt)*)?) $vis:vis struct $name:ident $($body:tt)+) => {
        $crate::def_lt_a_b! {
            [$($a)?] [$($b)?]
            $crate::wrap_bin_op { trait: ($crate::std_ops_Sub)::sub, kind: ref_rhs_rewrap(&Self), item: [$name] [$($($bound)*)?] [$($body)+] }
        }
    };
    ((& $($a:lifetime)? self, $Rhs:ty $(where $($bound:tt)*)?) $vis:vis struct $name:ident $($body:tt)+) => {
        $crate::def_lt_a! {
            [$($a)?]
            $crate::wrap_bin_op { trait: ($crate::std_ops_Sub)::sub, kind: ref_rhs_rewrap($Rhs), item: [$name] [] [] [$($($bound)*)?] [$($body)+] }
        }
    };
    ((<$($lt:lifetime),+ $(, $($T:ident),+)? $(,)?> & $($a:lifetime)? self, $Rhs:ty $(where $($bound:tt)*)?) $vis:vis struct $name:ident $($body:tt)+) => {
        $crate::def_lt_a! {
            [$($a)?]
            $crate::wrap_bin_op { trait: ($crate::std_ops_Sub)::sub, kind: ref_rhs_rewrap($Rhs), item: [$name] [ < $($lt),+ > ] [ $( < $($T),+ > )? ] [$($($bound)*)?] [$($body)+] }
        }
    };
    ((<$($($T:ident),+ $(,)?)?> & $($a:lifetime)? self, $Rhs:ty $(where $($bound:tt)*)?) $vis:vis struct $name:ident $($body:tt)+) => {
        $crate::def_lt_a! {
            [$($a)?]
            $crate::wrap_bin_op! { trait: ($crate::std_ops_Sub)::sub, kind: ref_rhs_rewrap($Rhs), item: [$name] [] [ $( < $($T),+ > )? ] [$($($bound)*)?] [$($body)+] }
        }
    };
    ((&Self $(where $($bound:tt)*)?) $vis:vis struct $name:ident $($body:tt)+) => {
        $crate::wrap_bin_op! { trait: ($crate::std_ops_Sub)::sub, kind: rhs_rewrap(&Self), item: ['newtype_derive] [$name] [$($($bound)*)?] [$($body)+] }
    };
    ((& $a:lifetime Self $(where $($bound:tt)*)?) $vis:vis struct $name:ident $($body:tt)+) => {
        $crate::wrap_bin_op! { trait: ($crate::std_ops_Sub)::sub, kind: rhs_rewrap(&Self), item: [$a] [$name] [$($($bound)*)?] [$($body)+] }
    };
    (($Rhs:ty $(where $($bound:tt)*)?) $vis:vis struct $name:ident $($body:tt)+) => {
        $crate::wrap_bin_op! { trait: ($crate::std_ops_Sub)::sub, kind: rhs_rewrap($Rhs), item: [$name] [] [] [$($($bound)*)?] [$($body)+] }
    };
    ((<$($lt:lifetime),+ $(, $($T:ident),+)? $(,)?> $Rhs:ty $(where $($bound:tt)*)?) $vis:vis struct $name:ident $($body:tt)+) => {
        $crate::wrap_bin_op! { trait: ($crate::std_ops_Sub)::sub, kind: rhs_rewrap($Rhs), item: [$name] [ < $($lt),+ > ] [ $( < $($T),+ > )? ] [$($($bound)*)?] [$($body)+] }
    };
    ((<$($($T:ident),+ $(,)?)?> $Rhs:ty $(where $($bound:tt)*)?) $vis:vis struct $name:ident $($body:tt)+) => {
        $crate::wrap_bin_op! { trait: ($crate::std_ops_Sub)::sub, kind: rhs_rewrap($Rhs), item: [$name] [] [ $( < $($T),+ > )? ] [$($($bound)*)?] [$($body)+] }
    };
}

#[macro_export]
macro_rules! NewtypeShl {
    ((* $(where $($bound:tt)*)?) $vis:vis struct $name:ident $($body:tt)+) => {
        $crate::NewtypeShl! { ($(where $($bound)*)?) $vis struct $name $($body)+ }
        $crate::NewtypeShl! { (&self $(where $($bound)*)?) $vis struct $name $($body)+ }
        $crate::NewtypeShl! { (&Self $(where $($bound)*)?) $vis struct $name $($body)+ }
        $crate::NewtypeShl! { (&self, &Self $(where $($bound)*)?) $vis struct $name $($body)+ }
    };
    (($(where $($bound:tt)*)?) $vis:vis struct $name:ident $($body:tt)+) => {
        $crate::wrap_bin_op! { trait: ($crate::std_ops_Shl)::shl, kind: simple, item: [$name] [$($($bound)*)?] [$($body)+] }
    };
    ((&self $(where $($bound:tt)*)?) $vis:vis struct $name:ident $($body:tt)+) => {
        $crate::wrap_bin_op! { ['newtype_derive] trait: ($crate::std_ops_Shl)::shl, kind: simple_ref, item: [$name] [$($($bound)*)?] [$($body)+] }
    };
    ((& $($a:lifetime)? self $(where $($bound:tt)*)?) $vis:vis struct $name:ident $($body:tt)+) => {
        $crate::def_lt_a! {
            [$($a)?]
            $crate::wrap_bin_op { trait: ($crate::std_ops_Shl)::shl, kind: simple_ref, item: [$name] [$($($bound)*)?] [$($body)+] }
        }
    };
    ((& $($a:lifetime)? self, & $($b:lifetime)? Self $(where $($bound:tt)*)?) $vis:vis struct $name:ident $($body:tt)+) => {
        $crate::def_lt_a_b! {
            [$($a)?] [$($b)?]
            $crate::wrap_bin_op { trait: ($crate::std_ops_Shl)::shl, kind: ref_rhs_rewrap(&Self), item: [$name] [$($($bound)*)?] [$($body)+] }
        }
    };
    ((& $($a:lifetime)? self, $Rhs:ty $(where $($bound:tt)*)?) $vis:vis struct $name:ident $($body:tt)+) => {
        $crate::def_lt_a! {
            [$($a)?]
            $crate::wrap_bin_op { trait: ($crate::std_ops_Shl)::shl, kind: ref_rhs_rewrap($Rhs), item: [$name] [] [] [$($($bound)*)?] [$($body)+] }
        }
    };
    ((<$($lt:lifetime),+ $(, $($T:ident),+)? $(,)?> & $($a:lifetime)? self, $Rhs:ty $(where $($bound:tt)*)?) $vis:vis struct $name:ident $($body:tt)+) => {
        $crate::def_lt_a! {
            [$($a)?]
            $crate::wrap_bin_op { trait: ($crate::std_ops_Shl)::shl, kind: ref_rhs_rewrap($Rhs), item: [$name] [ < $($lt),+ > ] [ $( < $($T),+ > )? ] [$($($bound)*)?] [$($body)+] }
        }
    };
    ((<$($($T:ident),+ $(,)?)?> & $($a:lifetime)? self, $Rhs:ty $(where $($bound:tt)*)?) $vis:vis struct $name:ident $($body:tt)+) => {
        $crate::def_lt_a! {
            [$($a)?]
            $crate::wrap_bin_op { trait: ($crate::std_ops_Shl)::shl, kind: ref_rhs_rewrap($Rhs), item: [$name] [] [ $( < $($T),+ > )? ] [$($($bound)*)?] [$($body)+] }
        }
    };
    ((&Self $(where $($bound:tt)*)?) $vis:vis struct $name:ident $($body:tt)+) => {
        $crate::wrap_bin_op! { trait: ($crate::std_ops_Shl)::shl, kind: rhs_rewrap(&Self), item: ['newtype_derive] [$name] [$($($bound)*)?] [$($body)+] }
    };
    ((& $a:lifetime Self $(where $($bound:tt)*)?) $vis:vis struct $name:ident $($body:tt)+) => {
        $crate::wrap_bin_op! { trait: ($crate::std_ops_Shl)::shl, kind: rhs_rewrap(&Self), item: [$a] [$name] [$($($bound)*)?] [$($body)+] }
    };
    (($Rhs:ty $(where $($bound:tt)*)?) $vis:vis struct $name:ident $($body:tt)+) => {
        $crate::wrap_bin_op! { trait: ($crate::std_ops_Shl)::shl, kind: rhs_rewrap($Rhs), item: [$name] [] [] [$($($bound)*)?] [$($body)+] }
    };
    ((<$($lt:lifetime),+ $(, $($T:ident),+)? $(,)?> $Rhs:ty $(where $($bound:tt)*)?) $vis:vis struct $name:ident $($body:tt)+) => {
        $crate::wrap_bin_op! { trait: ($crate::std_ops_Shl)::shl, kind: rhs_rewrap($Rhs), item: [$name] [ < $($lt),+ > ] [ $( < $($T),+ > )? ] [$($($bound)*)?] [$($body)+] }
    };
    ((<$($($T:ident),+ $(,)?)?> $Rhs:ty $(where $($bound:tt)*)?) $vis:vis struct $name:ident $($body:tt)+) => {
        $crate::wrap_bin_op! { trait: ($crate::std_ops_Shl)::shl, kind: rhs_rewrap($Rhs), item: [$name] [] [ $( < $($T),+ > )? ] [$($($bound)*)?] [$($body)+] }
    };
}

#[macro_export]
macro_rules! NewtypeShr {
    ((* $(where $($bound:tt)*)?) $vis:vis struct $name:ident $($body:tt)+) => {
        $crate::NewtypeShr! { ($(where $($bound)*)?) $vis struct $name $($body)+ }
        $crate::NewtypeShr! { (&self $(where $($bound)*)?) $vis struct $name $($body)+ }
        $crate::NewtypeShr! { (&Self $(where $($bound)*)?) $vis struct $name $($body)+ }
        $crate::NewtypeShr! { (&self, &Self $(where $($bound)*)?) $vis struct $name $($body)+ }
    };
    (($(where $($bound:tt)*)?) $vis:vis struct $name:ident $($body:tt)+) => {
        $crate::wrap_bin_op! { trait: ($crate::std_ops_Shr)::shr, kind: simple, item: [$name] [$($($bound)*)?] [$($body)+] }
    };
    ((&self $(where $($bound:tt)*)?) $vis:vis struct $name:ident $($body:tt)+) => {
        $crate::wrap_bin_op! { ['newtype_derive] trait: ($crate::std_ops_Shr)::shr, kind: simple_ref, item: [$name] [$($($bound)*)?] [$($body)+] }
    };
    ((& $($a:lifetime)? self $(where $($bound:tt)*)?) $vis:vis struct $name:ident $($body:tt)+) => {
        $crate::def_lt_a! {
            [$($a)?]
            $crate::wrap_bin_op { trait: ($crate::std_ops_Shr)::shr, kind: simple_ref, item: [$name] [$($($bound)*)?] [$($body)+] }
        }
    };
    ((& $($a:lifetime)? self, & $($b:lifetime)? Self $(where $($bound:tt)*)?) $vis:vis struct $name:ident $($body:tt)+) => {
        $crate::def_lt_a_b! {
            [$($a)?] [$($b)?]
            $crate::wrap_bin_op { trait: ($crate::std_ops_Shr)::shr, kind: ref_rhs_rewrap(&Self), item: [$name] [$($($bound)*)?] [$($body)+] }
        }
    };
    ((& $($a:lifetime)? self, $Rhs:ty $(where $($bound:tt)*)?) $vis:vis struct $name:ident $($body:tt)+) => {
        $crate::def_lt_a! {
            [$($a)?]
            $crate::wrap_bin_op { trait: ($crate::std_ops_Shr)::shr, kind: ref_rhs_rewrap($Rhs), item: [$name] [] [] [$($($bound)*)?] [$($body)+] }
        }
    };
    ((<$($lt:lifetime),+ $(, $($T:ident),+)? $(,)?> & $($a:lifetime)? self, $Rhs:ty $(where $($bound:tt)*)?) $vis:vis struct $name:ident $($body:tt)+) => {
        $crate::def_lt_a! {
            [$($a)?]
            $crate::wrap_bin_op { trait: ($crate::std_ops_Shr)::shr, kind: ref_rhs_rewrap($Rhs), item: [$name] [ < $($lt),+ > ] [ $( < $($T),+ > )? ] [$($($bound)*)?] [$($body)+] }
        }
    };
    ((<$($($T:ident),+ $(,)?)?> & $($a:lifetime)? self, $Rhs:ty $(where $($bound:tt)*)?) $vis:vis struct $name:ident $($body:tt)+) => {
        $crate::def_lt_a! {
            [$($a)?]
            $crate::wrap_bin_op { trait: ($crate::std_ops_Shr)::shr, kind: ref_rhs_rewrap($Rhs), item: [$name] [] [ $( < $($T),+ > )? ] [$($($bound)*)?] [$($body)+] }
        }
    };
    ((&Self $(where $($bound:tt)*)?) $vis:vis struct $name:ident $($body:tt)+) => {
        $crate::wrap_bin_op! { trait: ($crate::std_ops_Shr)::shr, kind: rhs_rewrap(&Self), item: ['newtype_derive] [$name] [$($($bound)*)?] [$($body)+] }
    };
    ((& $a:lifetime Self $(where $($bound:tt)*)?) $vis:vis struct $name:ident $($body:tt)+) => {
        $crate::wrap_bin_op! { trait: ($crate::std_ops_Shr)::shr, kind: rhs_rewrap(&Self), item: [$a] [$name] [$($($bound)*)?] [$($body)+] }
    };
    (($Rhs:ty $(where $($bound:tt)*)?) $vis:vis struct $name:ident $($body:tt)+) => {
        $crate::wrap_bin_op! { trait: ($crate::std_ops_Shr)::shr, kind: rhs_rewrap($Rhs), item: [$name] [] [] [$($($bound)*)?] [$($body)+] }
    };
    ((<$($lt:lifetime),+ $(, $($T:ident),+)? $(,)?> $Rhs:ty $(where $($bound:tt)*)?) $vis:vis struct $name:ident $($body:tt)+) => {
        $crate::wrap_bin_op! { trait: ($crate::std_ops_Shr)::shr, kind: rhs_rewrap($Rhs), item: [$name] [ < $($lt),+ > ] [ $( < $($T),+ > )? ] [$($($bound)*)?] [$($body)+] }
    };
    ((<$($($T:ident),+ $(,)?)?> $Rhs:ty $(where $($bound:tt)*)?) $vis:vis struct $name:ident $($body:tt)+) => {
        $crate::wrap_bin_op! { trait: ($crate::std_ops_Shr)::shr, kind: rhs_rewrap($Rhs), item: [$name] [] [ $( < $($T),+ > )? ] [$($($bound)*)?] [$($body)+] }
    };
}

#[doc(hidden)]
#[macro_export]
macro_rules! wrap_un_op {
    (
        trait: ($($tr:tt)*)::$meth:ident,
        kind: simple,
        item: [$name:ident] [$($bound:tt)*] [$($body:tt)+]
    ) => {
        $crate::generics_parse! {
            $crate::wrap_un_op {
                generics_parse_done
                [
                    trait: ($($tr)*)::$meth,
                    kind: simple,
                    item: [$name] [$($bound)*]
                ]
            }
            $($body)+
        }
    };
    (
        generics_parse_done
        [
            trait: ($($tr:tt)*)::$meth:ident,
            kind: simple,
            item: [$name:ident] [$($bound:tt)*]
        ]
        [$($g:tt)*] [$($r:tt)*] [$($w:tt)*]
        ($(pub)? $t0:ty $(, $(pub)? $phantom:ty)* $(,)?);
    ) => {
        $crate::generics_concat! {
            $crate::wrap_un_op {
                generics_concat_done
                [
                    trait: ($($tr)*)::$meth,
                    kind: simple,
                    item: [$name] [$t0] [$([$phantom])*]
                ]
            }
            [$($g)*] [$($r)*] [$($w)*],
            [] [] [where $($bound)*]
        }
    };
    (
        generics_concat_done
        [
            trait: ($($tr:tt)*)::$meth:ident,
            kind: simple,
            item: [$name:ident] [$t0:ty] [$([$phantom:ty])*]
        ]
        [$($g:tt)*] [$($r:tt)*] [$($w:tt)*]
    ) => {
        $crate::as_item! {
            impl $($g)* $($tr)* for $name $($r)* $($w)* {
                type Output = Self;
                fn $meth(self) -> Self {
                    $name(
                        <$t0 as $($tr)*>::$meth(self.0)
                        $(, <$phantom as $crate::std_default_Default>::default())*
                    )
                }
            }
        }
    };

    (
        trait: ($($tr:tt)*)::$meth:ident,
        kind: simple_ref,
        item: [$a:lifetime] [$name:ident] [$($bound:tt)*] [$($body:tt)+]
    ) => {
        $crate::generics_parse! {
            $crate::wrap_un_op {
                generics_parse_done
                [
                    trait: ($($tr)*)::$meth,
                    kind: simple_ref,
                    item: [$a] [$name] [$($bound)*]
                ]
            }
            $($body)+
        }
    };
    (
        generics_parse_done
        [
            trait: ($($tr:tt)*)::$meth:ident,
            kind: simple_ref,
            item: [$a:lifetime] [$name:ident] [$($bound:tt)*]
        ]
        [$($g:tt)*] [$($r:tt)*] [$($w:tt)*]
        ($(pub)? $t0:ty $(, $(pub)? $phantom:ty)* $(,)?);
    ) => {
        $crate::generics_concat! {
            $crate::wrap_un_op {
                generics_concat_done
                [
                    trait: ($($tr)*)::$meth,
                    kind: simple_ref,
                    item: [$a] [$name] [$t0] [$([$phantom])*]
                ]
            }
            [ < $a > ] [] [],
            [$($g)*] [$($r)*] [$($w)*],
            [] [] [where $($bound)*]
        }
    };
    (
        generics_concat_done
        [
            trait: ($($tr:tt)*)::$meth:ident,
            kind: simple_ref,
            item: [$a:lifetime] [$name:ident] [$t0:ty] [$([$phantom:ty])*]
        ]
        [$($g:tt)*] [$($r:tt)*] [$($w:tt)*]
    ) => {
        $crate::as_item! {
            impl $($g)* $($tr)* for & $a $name $($r)* $($w)* {
                type Output = $name $($r)*;
                fn $meth(self) -> $name $($r)* {
                    $name(
                        <$t0 as $($tr)*>::$meth(self.0)
                        $(, <$phantom as $crate::std_default_Default>::default())*
                    )
                }
            }
        }
    };
}

#[macro_export]
macro_rules! NewtypeNeg {
    ((* $(where $($bound:tt)*)?) $vis:vis struct $name:ident $($body:tt)+) => {
        $crate::NewtypeNeg! { ($(where $($bound)*)?) $vis struct $name $($body)+ }
        $crate::NewtypeNeg! { (&self $(where $($bound)*)?) $vis struct $name $($body)+ }
    };
    (($(where $($bound:tt)*)?) $vis:vis struct $name:ident $($body:tt)+) => {
        $crate::wrap_un_op! { trait: ($crate::std_ops_Neg)::neg, kind: simple, item: [$name] [$($($bound)*)?] [$($body)+] }
    };
    ((&self $(where $($bound:tt)*)?) $vis:vis struct $name:ident $($body:tt)+) => {
        $crate::wrap_un_op! { trait: ($crate::std_ops_Neg)::neg, kind: simple_ref, item: ['newtype_derive] [$name] [$($($bound)*)?] [$($body)+] }
    };
}

#[macro_export]
macro_rules! NewtypeNot {
    ((* $(where $($bound:tt)*)?) $vis:vis struct $name:ident $($body:tt)+) => {
        $crate::NewtypeNot! { ($(where $($bound)*)?) $vis struct $name $($body)+ }
        $crate::NewtypeNot! { (&self $(where $($bound)*)?) $vis struct $name $($body)+ }
    };
    (($(where $($bound:tt)*)?) $vis:vis struct $name:ident $($body:tt)+) => {
        $crate::wrap_un_op! { trait: ($crate::std_ops_Not)::not, kind: simple, item: [$name] [$($($bound)*)?] [$($body)+] }
    };
    ((&self $(where $($bound:tt)*)?) $vis:vis struct $name:ident $($body:tt)+) => {
        $crate::wrap_un_op! { trait: ($crate::std_ops_Not)::not, kind: simple_ref, item: ['newtype_derive] [$name] [$($($bound)*)?] [$($body)+] }
    };
}

#[macro_export]
macro_rules! NewtypeDeref {
    (($(where $($bound:tt)*)?) $vis:vis struct $name:ident $($body:tt)+) => {
        $crate::generics_parse! {
            $crate::NewtypeDeref_impl {
                generics_parse_done
                [$name] [$($($bound)*)?]
            }
            $($body)+
        }
    };
}

#[doc(hidden)]
#[macro_export]
macro_rules! NewtypeDeref_impl {
    (
        generics_parse_done
        [$name:ident] [$($bound:tt)*]
        [$($g:tt)*] [$($r:tt)*] [$($w:tt)*]
        ($(pub)? $t0:ty $(, $(pub)? $phantom:ty)* $(,)?);
    ) => {
        $crate::generics_concat! {
            $crate::NewtypeDeref_impl {
                generics_concat_done
                [$name] [$t0]
            }
            [$($g)*] [$($r)*] [$($w)*],
            [] [] [where $($bound)*]
        }
    };
    (
        generics_concat_done
        [$name:ident] [$t0:ty]
        [$($g:tt)*] [$($r:tt)*] [$($w:tt)*]
    ) => {
        impl $($g)* $crate::std_ops_Deref for $name $($r)* $($w)* {
            type Target = <$t0 as $crate::std_ops_Deref>::Target;

            fn deref(&self) -> &Self::Target { &self.0 }
        }
    };
}

#[macro_export]
macro_rules! NewtypeDerefMut {
    (($(where $($bound:tt)*)?) $vis:vis struct $name:ident $($body:tt)+) => {
        $crate::generics_parse! {
            $crate::NewtypeDerefMut_impl {
                generics_parse_done
                [$name] [$($($bound)*)?]
            }
            $($body)+
        }
    };
}

#[doc(hidden)]
#[macro_export]
macro_rules! NewtypeDerefMut_impl {
    (
        generics_parse_done
        [$name:ident] [$($bound:tt)*]
        [$($g:tt)*] [$($r:tt)*] [$($w:tt)*]
        ($(pub)? $t0:ty $(, $(pub)? $phantom:ty)* $(,)?);
    ) => {
        $crate::generics_concat! {
            $crate::NewtypeDerefMut_impl {
                generics_concat_done
                [$name] [$t0]
            }
            [$($g)*] [$($r)*] [$($w)*],
            [] [] [where $($bound)*]
        }
    };
    (
        generics_concat_done
        [$name:ident] [$t0:ty]
        [$($g:tt)*] [$($r:tt)*] [$($w:tt)*]
    ) => {
        impl $($g)* $crate::std_ops_DerefMut for $name $($r)* $($w)* {
            fn deref_mut(&mut self) -> &mut Self::Target { &mut self.0 }
        }
    };
}

#[macro_export]
macro_rules! NewtypeIndex {
    (($Index:ty $(where $($bound:tt)*)?) $vis:vis struct $name:ident $($body:tt)+) => {
        $crate::generics_parse! {
            $crate::NewtypeIndex_impl {
                generics_parse_done
                [$name] [] [] [$($($bound)*)?] [$Index]
            }
            $($body)+
        }
    };
    ((<$($lt:lifetime),+ $(, $($T:ident),+)? $(,)?> $Index:ty $(where $($bound:tt)*)?) $vis:vis struct $name:ident $($body:tt)+) => {
        $crate::generics_parse! {
            $crate::NewtypeIndex_impl {
                generics_parse_done
                [$name] [ < $($lt),+ > ] [ $( < $($T),+ > )? ] [$($($bound)*)?] [$Index]
            }
            $($body)+
        }
    };
    ((<$($($T:ident),+ $(,)?)?> $Index:ty $(where $($bound:tt)*)?) $vis:vis struct $name:ident $($body:tt)+) => {
        $crate::generics_parse! {
            $crate::NewtypeIndex_impl {
                generics_parse_done
                [$name] [] [ $( < $($T),+ > )? ] [$($($bound)*)?] [$Index]
            }
            $($body)+
        }
    };
}

#[doc(hidden)]
#[macro_export]
macro_rules! NewtypeIndex_impl {
    (
        generics_parse_done
        [$name:ident] [$($lt:tt)*] [$($T:tt)*] [$($bound:tt)*] [$Index:ty]
        [$($g:tt)*] [$($r:tt)*] [$($w:tt)*]
        ($(pub)? $t0:ty $(, $(pub)? $phantom:ty)* $(,)?);
    ) => {
        $crate::generics_concat! {
            $crate::NewtypeIndex_impl {
                generics_concat_done
                [$name] [$Index] [$t0]
            }
            [$($lt)*] [] [],
            [$($g)*] [$($r)*] [$($w)*],
            [$($T)*] [] [],
            [] [] [where $($bound)*]
        }
    };
    (
        generics_concat_done
        [$name:ident] [$Index:ty] [$t0:ty]
        [$($g:tt)*] [$($r:tt)*] [$($w:tt)*]
    ) => {
        impl $($g)* $crate::std_ops_Index<$Index> for $name $($r)* $($w)* {
            type Output = <$t0 as $crate::std_ops_Index<$Index>>::Output;

            fn index(&self, index: $Index) -> &Self::Output {
                <$t0 as $crate::std_ops_Index<$Index>>::index(&self.0, index)
            }
        }
    };
}

#[macro_export]
macro_rules! NewtypeIndexMut {
    (($Index:ty $(where $($bound:tt)*)?) $vis:vis struct $name:ident $($body:tt)+) => {
        $crate::generics_parse! {
            $crate::NewtypeIndexMut_impl {
                generics_parse_done
                [$name] [] [] [$($($bound)*)?] [$Index]
            }
            $($body)+
        }
    };
    ((<$($lt:lifetime),+ $(, $($T:ident),+)? $(,)?> $Index:ty $(where $($bound:tt)*)?) $vis:vis struct $name:ident $($body:tt)+) => {
        $crate::generics_parse! {
            $crate::NewtypeIndexMut_impl {
                generics_parse_done
                [$name] [ < $($lt),+ > ] [ $( < $($T),+ > )? ] [$($($bound)*)?] [$Index]
            }
            $($body)+
        }
    };
    ((<$($($T:ident),+ $(,)?)?> $Index:ty $(where $($bound:tt)*)?) $vis:vis struct $name:ident $($body:tt)+) => {
        $crate::generics_parse! {
            $crate::NewtypeIndexMut_impl {
                generics_parse_done
                [$name] [] [ $( < $($T),+ > )? ] [$($($bound)*)?] [$Index]
            }
            $($body)+
        }
    };
}

#[doc(hidden)]
#[macro_export]
macro_rules! NewtypeIndexMut_impl {
    (
        generics_parse_done
        [$name:ident] [$($lt:tt)*] [$($T:tt)*] [$($bound:tt)*] [$Index:ty]
        [$($g:tt)*] [$($r:tt)*] [$($w:tt)*]
        ($(pub)? $t0:ty $(, $(pub)? $phantom:ty)* $(,)?);
    ) => {
        $crate::generics_concat! {
            $crate::NewtypeIndexMut_impl {
                generics_concat_done
                [$name] [$Index] [$t0]
            }
            [$($lt)*] [] [],
            [$($g)*] [$($r)*] [$($w)*],
            [$($T)*] [] [],
            [] [] [where $($bound)*]
        }
    };
    (
        generics_concat_done
        [$name:ident] [$Index:ty] [$t0:ty]
        [$($g:tt)*] [$($r:tt)*] [$($w:tt)*]
    ) => {
        impl $($g)* $crate::std_ops_IndexMut<$Index> for $name $($r)* $($w)* {
            fn index_mut(&mut self, index: $Index) -> &mut Self::Output {
                <$t0 as $crate::std_ops_IndexMut<$Index>>::index_mut(&mut self.0, index)
            }
        }
    };
}

#[doc(hidden)]
#[macro_export]
macro_rules! wrap_fmt {
    ([$tr:path] [$name:ident] [$($bound:tt)*] [$($body:tt)+]) => {
        $crate::generics_parse! {
            $crate::wrap_fmt {
                generics_parse_done
                [$tr] [$name] [$($bound)*]
            }
            $($body)+
        }
    };
    (
        generics_parse_done
        [$tr:path] [$name:ident] [$($bound:tt)*]
        [$($g:tt)*] [$($r:tt)*] [$($w:tt)*]
        ($(pub)? $t0:ty $(, $(pub)? $phantom:ty)* $(,)?);
    ) => {
        $crate::generics_concat! {
            $crate::wrap_fmt {
                generics_concat_done
                [$tr] [$name] [$t0]
            }
            [$($g)*] [$($r)*] [$($w)*],
            [] [] [where $($bound)*]
        }
    };
    (
        generics_concat_done
        [$tr:path] [$name:ident] [$t0:ty]
        [$($g:tt)*] [$($r:tt)*] [$($w:tt)*]
    ) => {
        impl $($g)* $tr for $name $($r)* $($w)* {
            fn fmt(&self, f: &mut $crate::std_fmt_Formatter) -> $crate::std_fmt_Result {
                <$t0 as $tr>::fmt(&self.0, f)
            }
        }
    };
}

#[macro_export]
macro_rules! NewtypeBinary {
    (($(where $($bound:tt)*)?) $vis:vis struct $name:ident $($body:tt)+) => {
        $crate::wrap_fmt! { [$crate::std_fmt_Binary] [$name] [$($($bound)*)?] [$($body)+] }
    };
}

#[macro_export]
macro_rules! NewtypeDebug {
    (($(where $($bound:tt)*)?) $vis:vis struct $name:ident $($body:tt)+) => {
        $crate::wrap_fmt! { [$crate::std_fmt_Debug] [$name] [$($($bound)*)?] [$($body)+] }
    };
}

#[macro_export]
macro_rules! NewtypeDisplay {
    (($(where $($bound:tt)*)?) $vis:vis struct $name:ident $($body:tt)+) => {
        $crate::wrap_fmt! { [$crate::std_fmt_Display] [$name] [$($($bound)*)?] [$($body)+] }
    };
}

#[macro_export]
macro_rules! NewtypeLowerExp {
    (($(where $($bound:tt)*)?) $vis:vis struct $name:ident $($body:tt)+) => {
        $crate::wrap_fmt! { [$crate::std_fmt_LowerExp] [$name] [$($($bound)*)?] [$($body)+] }
    };
}

#[macro_export]
macro_rules! NewtypeLowerHex {
    (($(where $($bound:tt)*)?) $vis:vis struct $name:ident $($body:tt)+) => {
        $crate::wrap_fmt! { [$crate::std_fmt_LowerHex] [$name] [$($($bound)*)?] [$($body)+] }
    };
}

#[macro_export]
macro_rules! NewtypeOctal {
    (($(where $($bound:tt)*)?) $vis:vis struct $name:ident $($body:tt)+) => {
        $crate::wrap_fmt! { [$crate::std_fmt_Octal] [$name] [$($($bound)*)?] [$($body)+] }
    };
}

#[macro_export]
macro_rules! NewtypePointer {
    (($(where $($bound:tt)*)?) $vis:vis struct $name:ident $($body:tt)+) => {
        $crate::wrap_fmt! { [$crate::std_fmt_Pointer] [$name] [$($($bound)*)?] [$($body)+] }
    };
}

#[macro_export]
macro_rules! NewtypeUpperExp {
    (($(where $($bound:tt)*)?) $vis:vis struct $name:ident $($body:tt)+) => {
        $crate::wrap_fmt! { [$crate::std_fmt_UpperExp] [$name] [$($($bound)*)?] [$($body)+] }
    };
}

#[macro_export]
macro_rules! NewtypeUpperHex {
    (($(where $($bound:tt)*)?) $vis:vis struct $name:ident $($body:tt)+) => {
        $crate::wrap_fmt! { [$crate::std_fmt_UpperHex] [$name] [$($($bound)*)?] [$($body)+] }
    };
}
