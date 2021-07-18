// Copyright (c) 2015 macro-attr contributors.
// Copyright (c) 2021 Warlock <internalmike@gmail.com>
//
// Licensed under the MIT license (see LICENSE or <http://opensource.org
// /licenses/MIT>) or the Apache License, Version 2.0 (see LICENSE of
// <http://www.apache.org/licenses/LICENSE-2.0>), at your option. All
// files in the project carrying such notice may not be copied, modified,
// or distributed except according to those terms.

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
//! let mut arr = I32Array(vec![1, 2, 3]);
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
//! All these macros support generic newtype structs.
//!
//! ## Binary Arithmetic Operators
//!
//! Each of the binary arithmetic operators accept several deriving forms.
//! To use `Add` on a struct `T` as an example:
//!
//! | Attribute                 | Generated implementation                  |
//! |---------------------------|-------------------------------------------|
//! | `NewtypeAdd`              | `impl Add<T, Output=T> for T`             |
//! | `NewtypeAdd(&self)`       | `impl<'a> Add<&'a T, Output=T> for &'a T` |
//! | `NewtypeAdd(U)`           | `impl Add<U, Output=T> for T`             |
//! | `NewtypeAdd(&self, U)`    | `impl<'a> Add<U, Output=T> for &'a T`     |
//! | `NewtypeAdd(*)`           | All four combinations of `T` and `&T`     |
//!
//! The `*Assign` variants accept zero or one argument only. For example:
//!
//! | Attribute                 | Generated implementation                  |
//! |---------------------------|-------------------------------------------|
//! | `NewtypeAddAssign`        | `impl AddAssign<T> for T`                 |
//! | `NewtypeAddAssign(&Self)` | `impl<'a> Add<&'a T> for &'a T`           |
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
macro_rules! as_item {
    ($i:item) => {$i};
}

#[doc(hidden)]
#[macro_export]
macro_rules! wrap_bin_op {
    (
        trait: ($($tr:tt)*)::$meth:ident,
        kind: simple,
        item: [$name:ident] [$($body:tt)+]
    ) => {
        $crate::generics_parse! {
            $crate::wrap_bin_op {
                generics_parse_done
                [
                    trait: ($($tr)*)::$meth,
                    kind: simple,
                    item: [$name]
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
            item: [$name:ident]
        ]
        [$($g:tt)*] [$($r:tt)*] [$($w:tt)*] ($(pub)? $t:ty);
    ) => {
        $crate::as_item! {
            impl $($g)* $($tr)*<$name $($r)*> for $name $($r)* $($w)* {
                type Output = Self;
                fn $meth(self, rhs: Self) -> Self {
                    $name(<$t as $($tr)*<$t>>::$meth(self.0, rhs.0))
                }
            }
        }
    };

    (
        trait: ($($tr:tt)*)::$meth:ident,
        kind: simple_ref,
        item: [$name:ident] [$($body:tt)+]
    ) => {
        $crate::generics_parse! {
            $crate::wrap_bin_op {
                generics_parse_done
                [
                    trait: ($($tr)*)::$meth,
                    kind: simple_ref('newtype_derive),
                    item: [$name]
                ]
            }
            $($body)+
        }
    };
    (
        generics_parse_done
        [
            trait: ($($tr:tt)*)::$meth:ident,
            kind: simple_ref($a:lifetime),
            item: [$name:ident]
        ]
        [$($g:tt)*] [$($r:tt)*] [$($w:tt)*] ($(pub)? $t:ty);
    ) => {
        $crate::generics_concat! {
            $crate::wrap_bin_op {
                generics_concat_done
                [
                    trait: ($($tr)*)::$meth,
                    kind: simple_ref($a),
                    item: [$name] [$t]
                ]
            }
            [ < $a > ] [] [],
            [$($g)*] [$($r)*] [$($w)*],
        }
    };
    (
        generics_concat_done
        [
            trait: ($($tr:tt)*)::$meth:ident,
            kind: simple_ref($a:lifetime),
            item: [$name:ident] [$t:ty]
        ]
        [$($g:tt)*] [$($r:tt)*] [$($w:tt)*]
    ) => {
        $crate::as_item! {
            impl $($g)* $($tr)*<& $a $name $($r)*> for & $a $name $($r)* $($w)* {
                type Output = $name $($r)*;
                fn $meth(self, rhs: Self) -> $name $($r)* {
                    $name(<$t as $($tr)*<$t>>::$meth(self.0, rhs.0))
                }
            }
        }
    };

    (
        trait: ($($tr:tt)*)::$meth:ident,
        kind: rhs_rewrap(&Self),
        item: [$name:ident] [$($body:tt)+]
    ) => {
        $crate::generics_parse! {
            $crate::wrap_bin_op {
                generics_parse_done
                [
                    trait: ($($tr)*)::$meth,
                    kind: rhs_rewrap(&'newtype_derive Self),
                    item: [$name]
                ]
            }
            $($body)+
        }
    };
    (
        generics_parse_done
        [
            trait: ($($tr:tt)*)::$meth:ident,
            kind: rhs_rewrap(& $a:lifetime Self),
            item: [$name:ident]
        ]
        [$($g:tt)*] [$($r:tt)*] [$($w:tt)*] ($(pub)? $t:ty);
    ) => {
        $crate::generics_concat! {
            $crate::wrap_bin_op {
                generics_concat_done
                [
                    trait: ($($tr)*)::$meth,
                    kind: rhs_rewrap(& $a Self),
                    item: [$name] [$t]
                ]
            }
            [ < $a > ] [] [],
            [$($g)*] [$($r)*] [$($w)*]
        }
    };
    (
        generics_concat_done
        [
            trait: ($($tr:tt)*)::$meth:ident,
            kind: rhs_rewrap(& $a:lifetime Self),
            item: [$name:ident] [$t:ty]
        ]
        [$($g:tt)*] [$($r:tt)*] [$($w:tt)*]
    ) => {
        $crate::as_item! {
            impl $($g)* $($tr)*<& $a $name $($r)*> for $name $($r)* $($w)* {
                type Output = Self;
                fn $meth(self, rhs: & $a Self) -> Self {
                    $name(<$t as $($tr)*<&$t>>::$meth(self.0, &rhs.0))
                }
            }
        }
    };

    (
        trait: ($($tr:tt)*)::$meth:ident,
        kind: rhs_rewrap($rhs:ty),
        item: [$name:ident] [$($body:tt)+]
    ) => {
        $crate::generics_parse! {
            $crate::wrap_bin_op {
                generics_parse_done
                [
                    trait: ($($tr)*)::$meth,
                    kind: rhs_rewrap($rhs),
                    item: [$name]
                ]
            }
            $($body)+
        }
    };
    (
        generics_parse_done
        [
            trait: ($($tr:tt)*)::$meth:ident,
            kind: rhs_rewrap($rhs:ty),
            item: [$name:ident]
        ]
        [$($g:tt)*] [$($r:tt)*] [$($w:tt)*] ($(pub)? $t:ty);
    ) => {
        $crate::as_item! {
            impl $($g)* $($tr)*<$rhs> for $name $($r)* $($w)* {
                type Output = Self;
                fn $meth(self, rhs: $rhs) -> Self {
                    $name(<$t as $($tr)*<$rhs>>::$meth(self.0, rhs))
                }
            }
        }
    };

    (
        trait: ($($tr:tt)*)::$meth:ident,
        kind: ref_rhs_rewrap(Self),
        item: [$name:ident] [$($body:tt)+]
    ) => {
        $crate::generics_parse! {
            $crate::wrap_bin_op {
                generics_parse_done
                [
                    trait: ($($tr)*)::$meth,
                    kind: ref_rhs_rewrap('newtype_derive Self) ,
                    item: [$name]
                ]
            }
            $($body)+
        }
    };
    (
        generics_parse_done
        [
            trait: ($($tr:tt)*)::$meth:ident,
            kind: ref_rhs_rewrap($a:lifetime Self),
            item: [$name:ident]
        ]
        [$($g:tt)*] [$($r:tt)*] [$($w:tt)*] ($(pub)? $t:ty);
    ) => {
        $crate::generics_concat! {
            $crate::wrap_bin_op {
                generics_concat_done
                [
                    trait: ($($tr)*)::$meth,
                    kind: ref_rhs_rewrap($a Self),
                    item: [$name] [$t]
                ]
            }
            [ < $a > ] [] [],
            [$($g)*] [$($r)*] [$($w)*]
        }
    };
    (
        generics_concat_done
        [
            trait: ($($tr:tt)*)::$meth:ident,
            kind: ref_rhs_rewrap($a:lifetime Self),
            item: [$name:ident] [$t:ty]
        ]
        [$($g:tt)*] [$($r:tt)*] [$($w:tt)*]
    ) => {
        $crate::as_item! {
            impl $($g)* $($tr)*<$name $($r)*> for & $a $name $($r)* $($w)* {
                type Output = $name $($r)*;
                fn $meth(self, rhs: $name $($r)*) -> $name $($r)* {
                    $name(<$t as $($tr)*<$t>>::$meth(self.0, rhs.0))
                }
            }
        }
    };

    (
        trait: ($($tr:tt)*)::$meth:ident,
        kind: ref_rhs_rewrap($($rhs:tt)+),
        item: [$name:ident] [$($body:tt)+]
    ) => {
        $crate::generics_parse! {
            $crate::wrap_bin_op {
                rhs_generics_parse_done
                [
                    trait: ($($tr)*)::$meth,
                    kind: ref_rhs_rewrap,
                    item: [$name] [$($body)+]
                ]
            }
            $($rhs)+
        }
    };
    (
        rhs_generics_parse_done
        [
            trait: ($($tr:tt)*)::$meth:ident,
            kind: ref_rhs_rewrap,
            item: [$name:ident] [$($body:tt)+]
        ]
        [$($rhs_g:tt)*] [$($rhs_r:tt)*] [$($rhs_w:tt)*] $rhs:ty
    ) => {
        $crate::generics_parse! {
            $crate::wrap_bin_op {
                generics_parse_done
                [
                    trait: ($($tr)*)::$meth,
                    kind: ref_rhs_rewrap('newtype_derive $rhs),
                    item: [$name] [$($rhs_g)*] [$($rhs_r)*] [$($rhs_w)*]
                ]
            }
            $($body)+
        }
    };
    (
        generics_parse_done
        [
            trait: ($($tr:tt)*)::$meth:ident,
            kind: ref_rhs_rewrap($a:lifetime $rhs:ty),
            item: [$name:ident] [$($rhs_g:tt)*] [$($rhs_r:tt)*] [$($rhs_w:tt)*]
        ]
        [$($g:tt)*] [$($r:tt)*] [$($w:tt)*] ($(pub)? $t:ty);
    ) => {
        $crate::generics_concat! {
            $crate::wrap_bin_op {
                generics_concat_done
                [
                    trait: ($($tr)*)::$meth,
                    kind: ref_rhs_rewrap($a $rhs),
                    item: [$name] [$t]
                ]
            }
            [ < $a > ] [] [],
            [$($g)*] [$($r)*] [$($w)*],
            [$($rhs_g)*] [] [$($rhs_w)*]
        }
    };
    (
        generics_concat_done
        [
            trait: ($($tr:tt)*)::$meth:ident,
            kind: ref_rhs_rewrap($a:lifetime $rhs:ty),
            item: [$name:ident] [$t:ty]
        ]
        [$($g:tt)*] [$($r:tt)*] [$($w:tt)*]
    ) => {
        $crate::as_item! {
            impl $($g)* $($tr)*<$rhs> for & $a $name $($r)* $($w)* {
                type Output = $name $($r)*;
                fn $meth(self, rhs: $rhs) -> $name $($r)* {
                    $name(<$t as $($tr)*<$rhs>>::$meth(self.0, rhs))
                }
            }
        }
    };
}

#[macro_export]
macro_rules! NewtypeAdd {
    ((*) $vis:vis struct $name:ident $($body:tt)+) => {
        $crate::NewtypeAdd! { () $vis struct $name $($body)+ }
        $crate::NewtypeAdd! { (&self) $vis struct $name $($body)+ }
        $crate::NewtypeAdd! { (&Self) $vis struct $name $($body)+ }
        $crate::NewtypeAdd! { (&self, Self) $vis struct $name $($body)+ }
    };
    (() $vis:vis struct $name:ident $($body:tt)+) => {
        $crate::wrap_bin_op! { trait: ($crate::std_ops_Add)::add, kind: simple, item: [$name] [$($body)+] }
    };
    ((&self) $vis:vis struct $name:ident $($body:tt)+) => {
        $crate::wrap_bin_op! { trait: ($crate::std_ops_Add)::add, kind: simple_ref, item: [$name] [$($body)+] }
    };
    ((&self, Self) $vis:vis struct $name:ident $($body:tt)+) => {
        $crate::wrap_bin_op! { trait: ($crate::std_ops_Add)::add, kind: ref_rhs_rewrap(Self), item: [$name] [$($body)+] }
    };
    ((&self, $($rhs:tt)+) $vis:vis struct $name:ident $($body:tt)+) => {
        $crate::wrap_bin_op! { trait: ($crate::std_ops_Add)::add, kind: ref_rhs_rewrap($($rhs)+), item: [$name] [$($body)+] }
    };
    ((&Self) $vis:vis struct $name:ident $($body:tt)+) => {
        $crate::wrap_bin_op! { trait: ($crate::std_ops_Add)::add, kind: rhs_rewrap(&Self), item: [$name] [$($body)+] }
    };
    (($rhs:ty) $vis:vis struct $name:ident $($body:tt)+) => {
        $crate::wrap_bin_op! { trait: ($crate::std_ops_Add)::add, kind: rhs_rewrap($rhs), item: [$name] [$($body)+] }
    };
}

#[macro_export]
macro_rules! NewtypeBitAnd {
    ((*) $vis:vis struct $name:ident $($body:tt)+) => {
        $crate::NewtypeBitAnd! { () $vis struct $name $($body)+ }
        $crate::NewtypeBitAnd! { (&self) $vis struct $name $($body)+ }
        $crate::NewtypeBitAnd! { (&Self) $vis struct $name $($body)+ }
        $crate::NewtypeBitAnd! { (&self, Self) $vis struct $name $($body)+ }
    };
    (() $vis:vis struct $name:ident $($body:tt)+) => {
        $crate::wrap_bin_op! { trait: ($crate::std_ops_BitAnd)::bitand, kind: simple, item: [$name] [$($body)+] }
    };
    ((&self) $vis:vis struct $name:ident $($body:tt)+) => {
        $crate::wrap_bin_op! { trait: ($crate::std_ops_BitAnd)::bitand, kind: simple_ref, item: [$name] [$($body)+] }
    };
    ((&self, Self) $vis:vis struct $name:ident $($body:tt)+) => {
        $crate::wrap_bin_op! { trait: ($crate::std_ops_BitAnd)::bitand, kind: ref_rhs_rewrap(Self), item: [$name] [$($body)+] }
    };
    ((&self, $($rhs:tt)+) $vis:vis struct $name:ident $($body:tt)+) => {
        $crate::wrap_bin_op! { trait: ($crate::std_ops_BitAnd)::bitand, kind: ref_rhs_rewrap($($rhs)+), item: [$name] [$($body)+] }
    };
    ((&Self) $vis:vis struct $name:ident $($body:tt)+) => {
        $crate::wrap_bin_op! { trait: ($crate::std_ops_BitAnd)::bitand, kind: rhs_rewrap(&Self), item: [$name] [$($body)+] }
    };
    (($rhs:ty) $vis:vis struct $name:ident $($body:tt)+) => {
        $crate::wrap_bin_op! { trait: ($crate::std_ops_BitAnd)::bitand, kind: rhs_rewrap($rhs), item: [$name] [$($body)+] }
    };
}

#[macro_export]
macro_rules! NewtypeBitOr {
    ((*) $vis:vis struct $name:ident $($body:tt)+) => {
        $crate::NewtypeBitOr! { () $vis struct $name $($body)+ }
        $crate::NewtypeBitOr! { (&self) $vis struct $name $($body)+ }
        $crate::NewtypeBitOr! { (&Self) $vis struct $name $($body)+ }
        $crate::NewtypeBitOr! { (&self, Self) $vis struct $name $($body)+ }
    };
    (() $vis:vis struct $name:ident $($body:tt)+) => {
        $crate::wrap_bin_op! { trait: ($crate::std_ops_BitOr)::bitor, kind: simple, item: [$name] [$($body)+] }
    };
    ((&self) $vis:vis struct $name:ident $($body:tt)+) => {
        $crate::wrap_bin_op! { trait: ($crate::std_ops_BitOr)::bitor, kind: simple_ref, item: [$name] [$($body)+] }
    };
    ((&self, Self) $vis:vis struct $name:ident $($body:tt)+) => {
        $crate::wrap_bin_op! { trait: ($crate::std_ops_BitOr)::bitor, kind: ref_rhs_rewrap(Self), item: [$name] [$($body)+] }
    };
    ((&self, $($rhs:tt)+) $vis:vis struct $name:ident $($body:tt)+) => {
        $crate::wrap_bin_op! { trait: ($crate::std_ops_BitOr)::bitor, kind: ref_rhs_rewrap($($rhs)+), item: [$name] [$($body)+] }
    };
    ((&Self) $vis:vis struct $name:ident $($body:tt)+) => {
        $crate::wrap_bin_op! { trait: ($crate::std_ops_BitOr)::bitor, kind: rhs_rewrap(&Self), item: [$name] [$($body)+] }
    };
    (($rhs:ty) $vis:vis struct $name:ident $($body:tt)+) => {
        $crate::wrap_bin_op! { trait: ($crate::std_ops_BitOr)::bitor, kind: rhs_rewrap($rhs), item: [$name] [$($body)+] }
    };
}

#[macro_export]
macro_rules! NewtypeBitXor {
    ((*) $vis:vis struct $name:ident $($body:tt)+) => {
        $crate::NewtypeBitXor! { () $vis struct $name $($body)+ }
        $crate::NewtypeBitXor! { (&self) $vis struct $name $($body)+ }
        $crate::NewtypeBitXor! { (&Self) $vis struct $name $($body)+ }
        $crate::NewtypeBitXor! { (&self, Self) $vis struct $name $($body)+ }
    };
    (() $vis:vis struct $name:ident $($body:tt)+) => {
        $crate::wrap_bin_op! { trait: ($crate::std_ops_BitXor)::bitxor, kind: simple, item: [$name] [$($body)+] }
    };
    ((&self) $vis:vis struct $name:ident $($body:tt)+) => {
        $crate::wrap_bin_op! { trait: ($crate::std_ops_BitXor)::bitxor, kind: simple_ref, item: [$name] [$($body)+] }
    };
    ((&self, Self) $vis:vis struct $name:ident $($body:tt)+) => {
        $crate::wrap_bin_op! { trait: ($crate::std_ops_BitXor)::bitxor, kind: ref_rhs_rewrap(Self), item: [$name] [$($body)+] }
    };
    ((&self, $($rhs:tt)+) $vis:vis struct $name:ident $($body:tt)+) => {
        $crate::wrap_bin_op! { trait: ($crate::std_ops_BitXor)::bitxor, kind: ref_rhs_rewrap($($rhs)+), item: [$name] [$($body)+] }
    };
    ((&Self) $vis:vis struct $name:ident $($body:tt)+) => {
        $crate::wrap_bin_op! { trait: ($crate::std_ops_BitXor)::bitxor, kind: rhs_rewrap(&Self), item: [$name] [$($body)+] }
    };
    (($rhs:ty) $vis:vis struct $name:ident $($body:tt)+) => {
        $crate::wrap_bin_op! { trait: ($crate::std_ops_BitXor)::bitxor, kind: rhs_rewrap($rhs), item: [$name] [$($body)+] }
    };
}

#[macro_export]
macro_rules! NewtypeDiv {
    ((*) $vis:vis struct $name:ident $($body:tt)+) => {
        $crate::NewtypeDiv! { () $vis struct $name $($body)+ }
        $crate::NewtypeDiv! { (&self) $vis struct $name $($body)+ }
        $crate::NewtypeDiv! { (&Self) $vis struct $name $($body)+ }
        $crate::NewtypeDiv! { (&self, Self) $vis struct $name $($body)+ }
    };
    (() $vis:vis struct $name:ident $($body:tt)+) => {
        $crate::wrap_bin_op! { trait: ($crate::std_ops_Div)::div, kind: simple, item: [$name] [$($body)+] }
    };
    ((&self) $vis:vis struct $name:ident $($body:tt)+) => {
        $crate::wrap_bin_op! { trait: ($crate::std_ops_Div)::div, kind: simple_ref, item: [$name] [$($body)+] }
    };
    ((&self, Self) $vis:vis struct $name:ident $($body:tt)+) => {
        $crate::wrap_bin_op! { trait: ($crate::std_ops_Div)::div, kind: ref_rhs_rewrap(Self), item: [$name] [$($body)+] }
    };
    ((&self, $($rhs:tt)+) $vis:vis struct $name:ident $($body:tt)+) => {
        $crate::wrap_bin_op! { trait: ($crate::std_ops_Div)::div, kind: ref_rhs_rewrap($($rhs)+), item: [$name] [$($body)+] }
    };
    ((&Self) $vis:vis struct $name:ident $($body:tt)+) => {
        $crate::wrap_bin_op! { trait: ($crate::std_ops_Div)::div, kind: rhs_rewrap(&Self), item: [$name] [$($body)+] }
    };
    (($rhs:ty) $vis:vis struct $name:ident $($body:tt)+) => {
        $crate::wrap_bin_op! { trait: ($crate::std_ops_Div)::div, kind: rhs_rewrap($rhs), item: [$name] [$($body)+] }
    };
}

#[macro_export]
macro_rules! NewtypeMul {
    ((*) $vis:vis struct $name:ident $($body:tt)+) => {
        $crate::NewtypeMul! { () $vis struct $name $($body)+ }
        $crate::NewtypeMul! { (&self) $vis struct $name $($body)+ }
        $crate::NewtypeMul! { (&Self) $vis struct $name $($body)+ }
        $crate::NewtypeMul! { (&self, Self) $vis struct $name $($body)+ }
    };
    (() $vis:vis struct $name:ident $($body:tt)+) => {
        $crate::wrap_bin_op! { trait: ($crate::std_ops_Mul)::mul, kind: simple, item: [$name] [$($body)+] }
    };
    ((&self) $vis:vis struct $name:ident $($body:tt)+) => {
        $crate::wrap_bin_op! { trait: ($crate::std_ops_Mul)::mul, kind: simple_ref, item: [$name] [$($body)+] }
    };
    ((&self, Self) $vis:vis struct $name:ident $($body:tt)+) => {
        $crate::wrap_bin_op! { trait: ($crate::std_ops_Mul)::mul, kind: ref_rhs_rewrap(Self), item: [$name] [$($body)+] }
    };
    ((&self, $($rhs:tt)+) $vis:vis struct $name:ident $($body:tt)+) => {
        $crate::wrap_bin_op! { trait: ($crate::std_ops_Mul)::mul, kind: ref_rhs_rewrap($($rhs)+), item: [$name] [$($body)+] }
    };
    ((&Self) $vis:vis struct $name:ident $($body:tt)+) => {
        $crate::wrap_bin_op! { trait: ($crate::std_ops_Mul)::mul, kind: rhs_rewrap(&Self), item: [$name] [$($body)+] }
    };
    (($rhs:ty) $vis:vis struct $name:ident $($body:tt)+) => {
        $crate::wrap_bin_op! { trait: ($crate::std_ops_Mul)::mul, kind: rhs_rewrap($rhs), item: [$name] [$($body)+] }
    };
}

#[macro_export]
macro_rules! NewtypeRem {
    ((*) $vis:vis struct $name:ident $($body:tt)+) => {
        $crate::NewtypeRem! { () $vis struct $name $($body)+ }
        $crate::NewtypeRem! { (&self) $vis struct $name $($body)+ }
        $crate::NewtypeRem! { (&Self) $vis struct $name $($body)+ }
        $crate::NewtypeRem! { (&self, Self) $vis struct $name $($body)+ }
    };
    (() $vis:vis struct $name:ident $($body:tt)+) => {
        $crate::wrap_bin_op! { trait: ($crate::std_ops_Rem)::rem, kind: simple, item: [$name] [$($body)+] }
    };
    ((&self) $vis:vis struct $name:ident $($body:tt)+) => {
        $crate::wrap_bin_op! { trait: ($crate::std_ops_Rem)::rem, kind: simple_ref, item: [$name] [$($body)+] }
    };
    ((&self, Self) $vis:vis struct $name:ident $($body:tt)+) => {
        $crate::wrap_bin_op! { trait: ($crate::std_ops_Rem)::rem, kind: ref_rhs_rewrap(Self), item: [$name] [$($body)+] }
    };
    ((&self, $($rhs:tt)+) $vis:vis struct $name:ident $($body:tt)+) => {
        $crate::wrap_bin_op! { trait: ($crate::std_ops_Rem)::rem, kind: ref_rhs_rewrap($($rhs)+), item: [$name] [$($body)+] }
    };
    ((&Self) $vis:vis struct $name:ident $($body:tt)+) => {
        $crate::wrap_bin_op! { trait: ($crate::std_ops_Rem)::rem, kind: rhs_rewrap(&Self), item: [$name] [$($body)+] }
    };
    (($rhs:ty) $vis:vis struct $name:ident $($body:tt)+) => {
        $crate::wrap_bin_op! { trait: ($crate::std_ops_Rem)::rem, kind: rhs_rewrap($rhs), item: [$name] [$($body)+] }
    };
}

#[macro_export]
macro_rules! NewtypeSub {
    ((*) $vis:vis struct $name:ident $($body:tt)+) => {
        $crate::NewtypeSub! { () $vis struct $name $($body)+ }
        $crate::NewtypeSub! { (&self) $vis struct $name $($body)+ }
        $crate::NewtypeSub! { (&Self) $vis struct $name $($body)+ }
        $crate::NewtypeSub! { (&self, Self) $vis struct $name $($body)+ }
    };
    (() $vis:vis struct $name:ident $($body:tt)+) => {
        $crate::wrap_bin_op! { trait: ($crate::std_ops_Sub)::sub, kind: simple, item: [$name] [$($body)+] }
    };
    ((&self) $vis:vis struct $name:ident $($body:tt)+) => {
        $crate::wrap_bin_op! { trait: ($crate::std_ops_Sub)::sub, kind: simple_ref, item: [$name] [$($body)+] }
    };
    ((&self, Self) $vis:vis struct $name:ident $($body:tt)+) => {
        $crate::wrap_bin_op! { trait: ($crate::std_ops_Sub)::sub, kind: ref_rhs_rewrap(Self), item: [$name] [$($body)+] }
    };
    ((&self, $($rhs:tt)+) $vis:vis struct $name:ident $($body:tt)+) => {
        $crate::wrap_bin_op! { trait: ($crate::std_ops_Sub)::sub, kind: ref_rhs_rewrap($($rhs)+), item: [$name] [$($body)+] }
    };
    ((&Self) $vis:vis struct $name:ident $($body:tt)+) => {
        $crate::wrap_bin_op! { trait: ($crate::std_ops_Sub)::sub, kind: rhs_rewrap(&Self), item: [$name] [$($body)+] }
    };
    (($rhs:ty) $vis:vis struct $name:ident $($body:tt)+) => {
        $crate::wrap_bin_op! { trait: ($crate::std_ops_Sub)::sub, kind: rhs_rewrap($rhs), item: [$name] [$($body)+] }
    };
}

#[macro_export]
macro_rules! NewtypeShl {
    ((*) $vis:vis struct $name:ident $($body:tt)+) => {
        $crate::NewtypeShl! { () $vis struct $name $($body)+ }
        $crate::NewtypeShl! { (&self) $vis struct $name $($body)+ }
        $crate::NewtypeShl! { (&Self) $vis struct $name $($body)+ }
        $crate::NewtypeShl! { (&self, Self) $vis struct $name $($body)+ }
    };
    (() $vis:vis struct $name:ident $($body:tt)+) => {
        $crate::wrap_bin_op! { trait: ($crate::std_ops_Shl)::shl, kind: simple, item: [$name] [$($body)+] }
    };
    ((&self) $vis:vis struct $name:ident $($body:tt)+) => {
        $crate::wrap_bin_op! { trait: ($crate::std_ops_Shl)::shl, kind: simple_ref, item: [$name] [$($body)+] }
    };
    ((&self, Self) $vis:vis struct $name:ident $($body:tt)+) => {
        $crate::wrap_bin_op! { trait: ($crate::std_ops_Shl)::shl, kind: ref_rhs_rewrap(Self), item: [$name] [$($body)+] }
    };
    ((&self, $($rhs:tt)+) $vis:vis struct $name:ident $($body:tt)+) => {
        $crate::wrap_bin_op! { trait: ($crate::std_ops_Shl)::shl, kind: ref_rhs_rewrap($($rhs)+), item: [$name] [$($body)+] }
    };
    ((&Self) $vis:vis struct $name:ident $($body:tt)+) => {
        $crate::wrap_bin_op! { trait: ($crate::std_ops_Shl)::shl, kind: rhs_rewrap(&Self), item: [$name] [$($body)+] }
    };
    (($rhs:ty) $vis:vis struct $name:ident $($body:tt)+) => {
        $crate::wrap_bin_op! { trait: ($crate::std_ops_Shl)::shl, kind: rhs_rewrap($rhs), item: [$name] [$($body)+] }
    };
}

#[macro_export]
macro_rules! NewtypeShr {
    ((*) $vis:vis struct $name:ident $($body:tt)+) => {
        $crate::NewtypeShr! { () $vis struct $name $($body)+ }
        $crate::NewtypeShr! { (&self) $vis struct $name $($body)+ }
        $crate::NewtypeShr! { (&Self) $vis struct $name $($body)+ }
        $crate::NewtypeShr! { (&self, Self) $vis struct $name $($body)+ }
    };
    (() $vis:vis struct $name:ident $($body:tt)+) => {
        $crate::wrap_bin_op! { trait: ($crate::std_ops_Shr)::shr, kind: simple, item: [$name] [$($body)+] }
    };
    ((&self) $vis:vis struct $name:ident $($body:tt)+) => {
        $crate::wrap_bin_op! { trait: ($crate::std_ops_Shr)::shr, kind: simple_ref, item: [$name] [$($body)+] }
    };
    ((&self, Self) $vis:vis struct $name:ident $($body:tt)+) => {
        $crate::wrap_bin_op! { trait: ($crate::std_ops_Shr)::shr, kind: ref_rhs_rewrap(Self), item: [$name] [$($body)+] }
    };
    ((&self, $($rhs:tt)+) $vis:vis struct $name:ident $($body:tt)+) => {
        $crate::wrap_bin_op! { trait: ($crate::std_ops_Shr)::shr, kind: ref_rhs_rewrap($($rhs)+), item: [$name] [$($body)+] }
    };
    ((&Self) $vis:vis struct $name:ident $($body:tt)+) => {
        $crate::wrap_bin_op! { trait: ($crate::std_ops_Shr)::shr, kind: rhs_rewrap(&Self), item: [$name] [$($body)+] }
    };
    (($rhs:ty) $vis:vis struct $name:ident $($body:tt)+) => {
        $crate::wrap_bin_op! { trait: ($crate::std_ops_Shr)::shr, kind: rhs_rewrap($rhs), item: [$name] [$($body)+] }
    };
}

#[doc(hidden)]
#[macro_export]
macro_rules! wrap_un_op {
    (
        trait: ($($tr:tt)*)::$meth:ident,
        kind: simple,
        item: [$name:ident] [$($body:tt)+]
    ) => {
        $crate::generics_parse! {
            $crate::wrap_un_op {
                generics_parse_done
                [
                    trait: ($($tr)*)::$meth,
                    kind: simple,
                    item: [$name]
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
            item: [$name:ident]
        ]
        [$($g:tt)*] [$($r:tt)*] [$($w:tt)*] ($(pub)? $t:ty);
    ) => {
        $crate::as_item! {
            impl $($g)* $($tr)* for $name $($r)* $($w)* {
                type Output = Self;
                fn $meth(self) -> Self {
                    $name(<$t as $($tr)*>::$meth(self.0))
                }
            }
        }
    };

    (
        trait: ($($tr:tt)*)::$meth:ident,
        kind: simple_ref,
        item: [$name:ident] [$($body:tt)+]
    ) => {
        $crate::generics_parse! {
            $crate::wrap_un_op {
                generics_parse_done
                [
                    trait: ($($tr)*)::$meth,
                    kind: simple_ref('newtype_derive),
                    item: [$name]
                ]
            }
            $($body)+
        }
    };
    (
        generics_parse_done
        [
            trait: ($($tr:tt)*)::$meth:ident,
            kind: simple_ref($a:lifetime),
            item: [$name:ident]
        ]
        [$($g:tt)*] [$($r:tt)*] [$($w:tt)*] ($(pub)? $t:ty);
    ) => {
        $crate::generics_concat! {
            $crate::wrap_un_op {
                generics_concat_done
                [
                    trait: ($($tr)*)::$meth,
                    kind: simple_ref($a),
                    item: [$name] [$t]
                ]
            }
            [ < $a > ] [] [],
            [$($g)*] [$($r)*] [$($w)*]
        }
    };
    (
        generics_concat_done
        [
            trait: ($($tr:tt)*)::$meth:ident,
            kind: simple_ref($a:lifetime),
            item: [$name:ident] [$t:ty]
        ]
        [$($g:tt)*] [$($r:tt)*] [$($w:tt)*]
    ) => {
        $crate::as_item! {
            impl $($g)* $($tr)* for & $a $name $($r)* $($w)* {
                type Output = $name $($r)*;
                fn $meth(self) -> $name $($r)* {
                    $name(<$t as $($tr)*>::$meth(self.0))
                }
            }
        }
    };
}

#[macro_export]
macro_rules! NewtypeNeg {
    ((*) $vis:vis struct $name:ident $($body:tt)+) => {
        $crate::NewtypeNeg! { () $vis struct $name $($body)+ }
        $crate::NewtypeNeg! { (&self) $vis struct $name $($body)+ }
    };
    (() $vis:vis struct $name:ident $($body:tt)+) => {
        $crate::wrap_un_op! { trait: ($crate::std_ops_Neg)::neg, kind: simple, item: [$name] [$($body)+] }
    };
    ((&self) $vis:vis struct $name:ident $($body:tt)+) => {
        $crate::wrap_un_op! { trait: ($crate::std_ops_Neg)::neg, kind: simple_ref, item: [$name] [$($body)+] }
    };
}

#[macro_export]
macro_rules! NewtypeNot {
    ((*) $vis:vis struct $name:ident $($body:tt)+) => {
        $crate::NewtypeNot! { () $vis struct $name $($body)+ }
        $crate::NewtypeNot! { (&self) $vis struct $name $($body)+ }
    };
    (() $vis:vis struct $name:ident $($body:tt)+) => {
        $crate::wrap_un_op! { trait: ($crate::std_ops_Not)::not, kind: simple, item: [$name] [$($body)+] }
    };
    ((&self) $vis:vis struct $name:ident $($body:tt)+) => {
        $crate::wrap_un_op! { trait: ($crate::std_ops_Not)::not, kind: simple_ref, item: [$name] [$($body)+] }
    };
}

#[macro_export]
macro_rules! NewtypeDeref {
    (() $vis:vis struct $name:ident $($body:tt)+) => {
        $crate::generics_parse! {
            $crate::NewtypeDeref_impl {
                generics_parse_done
                [ $vis struct $name ]
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
        [ $vis:vis struct $name:ident ]
        [$($g:tt)*] [$($r:tt)*] [$($w:tt)*] ($(pub)? $t0:ty);
    ) => {
        impl $($g)* $crate::std_ops_Deref for $name $($r)* $($w)* {
            type Target = <$t0 as $crate::std_ops_Deref>::Target;

            fn deref(&self) -> &Self::Target { &self.0 }
        }
    };
}

#[macro_export]
macro_rules! NewtypeDerefMut {
    (() $vis:vis struct $name:ident $($body:tt)+) => {
        $crate::generics_parse! {
            $crate::NewtypeDerefMut_impl {
                generics_parse_done
                [ $vis struct $name ]
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
        [ $vis:vis struct $name:ident ]
        [$($g:tt)*] [$($r:tt)*] [$($w:tt)*] ($(pub)? $t0:ty);
    ) => {
        impl $($g)* $crate::std_ops_DerefMut for $name $($r)* $($w)* {
            fn deref_mut(&mut self) -> &mut Self::Target { &mut self.0 }
        }
    };
}

#[macro_export]
macro_rules! NewtypeIndex {
    (($Index:ty) $vis:vis struct $name:ident $($body:tt)+) => {
        $crate::generics_parse! {
            $crate::NewtypeIndex_impl {
                generics_parse_done
                [($Index) $vis struct $name ]
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
        [($Index:ty) $vis:vis struct $name:ident ]
        [$($g:tt)*] [$($r:tt)*] [$($w:tt)*] ($(pub)? $t0:ty);
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
    (($Index:ty) $vis:vis struct $name:ident $($body:tt)+) => {
        $crate::generics_parse! {
            $crate::NewtypeIndexMut_impl {
                generics_parse_done
                [($Index) $vis struct $name ]
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
        [($Index:ty) $vis:vis struct $name:ident ]
        [$($g:tt)*] [$($r:tt)*] [$($w:tt)*] ($(pub)? $t0:ty);
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
    ([$tr:path] [$name:ident] [$($body:tt)+]) => {
        $crate::generics_parse! {
            $crate::wrap_fmt {
                generics_parse_done
                [$tr] [$name]
            }
            $($body)+
        }
    };
    (
        generics_parse_done
        [$tr:path] [$name:ident]
        [$($g:tt)*] [$($r:tt)*] [$($w:tt)*] ($(pub)? $t0:ty);
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
    (() $vis:vis struct $name:ident $($body:tt)+) => {
        $crate::wrap_fmt! { [$crate::std_fmt_Binary] [$name] [$($body)+] }
    };
}

#[macro_export]
macro_rules! NewtypeDebug {
    (() $vis:vis struct $name:ident $($body:tt)+) => {
        $crate::wrap_fmt! { [$crate::std_fmt_Debug] [$name] [$($body)+] }
    };
}

#[macro_export]
macro_rules! NewtypeDisplay {
    (() $vis:vis struct $name:ident $($body:tt)+) => {
        $crate::wrap_fmt! { [$crate::std_fmt_Display] [$name] [$($body)+] }
    };
}

#[macro_export]
macro_rules! NewtypeLowerExp {
    (() $vis:vis struct $name:ident $($body:tt)+) => {
        $crate::wrap_fmt! { [$crate::std_fmt_LowerExp] [$name] [$($body)+] }
    };
}

#[macro_export]
macro_rules! NewtypeLowerHex {
    (() $vis:vis struct $name:ident $($body:tt)+) => {
        $crate::wrap_fmt! { [$crate::std_fmt_LowerHex] [$name] [$($body)+] }
    };
}

#[macro_export]
macro_rules! NewtypeOctal {
    (() $vis:vis struct $name:ident $($body:tt)+) => {
        $crate::wrap_fmt! { [$crate::std_fmt_Octal] [$name] [$($body)+] }
    };
}

#[macro_export]
macro_rules! NewtypePointer {
    (() $vis:vis struct $name:ident $($body:tt)+) => {
        $crate::wrap_fmt! { [$crate::std_fmt_Pointer] [$name] [$($body)+] }
    };
}

#[macro_export]
macro_rules! NewtypeUpperExp {
    (() $vis:vis struct $name:ident $($body:tt)+) => {
        $crate::wrap_fmt! { [$crate::std_fmt_UpperExp] [$name] [$($body)+] }
    };
}

#[macro_export]
macro_rules! NewtypeUpperHex {
    (() $vis:vis struct $name:ident $($body:tt)+) => {
        $crate::wrap_fmt! { [$crate::std_fmt_UpperHex] [$name] [$($body)+] }
    };
}
