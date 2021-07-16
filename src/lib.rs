// Copyright (c) 2015 macro-attr contributors.
// Copyright (c) 2021 Warlock <internalmike@gmail.com>
//
// Licensed under the MIT license (see LICENSE or <http://opensource.org
// /licenses/MIT>) or the Apache License, Version 2.0 (see LICENSE of
// <http://www.apache.org/licenses/LICENSE-2.0>), at your option. All
// files in the project carrying such notice may not be copied, modified,
// or distributed except according to those terms.

//! This crate provides several macros for deriving implementations of various traits for "newtype" wrappers (*i.e.* tuple structs with a single element).  That is, given a tuple struct with exactly one field (*e.g.* `struct Buckets(i32)`), these macros will derive "obvious" implementations of traits such as `Add`, `Neg`, `Index`, `Deref`, `From`, etc.
//!
//! All of these macros are designed to be used with the [`macro-attr`](https://crates.io/crates/macro-attr) crate, though they can be used independent of it.
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
//!     #[derive(NewtypeFrom!, NewtypeAdd!, NewtypeMul!(i32))]
//!     pub struct Happy(i32);
//! }
//! #
//! # fn main() {
//! // Let's add some happy little ints.
//! let a = Happy::from(6);
//! let b = Happy::from(7);
//! let c = (a + b) * 3;
//! let d: i32 = c.into();
//! assert_eq!(d, 39);
//! # }
//! ```
//!
//! Create a "deref-transparent" wrapper around a type:
//!
//! ```rust
//! use macro_attr_2018::macro_attr;
//! use newtype_derive_2018::*;
//!
//! macro_attr! {
//!     #[derive(NewtypeFrom!,
//!         NewtypeDeref!, NewtypeDerefMut!,
//!         NewtypeIndex!(usize), NewtypeIndexMut!(usize)
//!         )]
//!     pub struct I32Array(Vec<i32>);
//! }
//!
//! # fn main() {
//! let mut arr = I32Array::from(vec![1, 2, 3]);
//! arr.push(4);
//! arr[2] = 5;
//! assert_eq!(&**arr, &[1, 2, 5, 4]);
//! assert_eq!(arr.len(), 4);
//! # }
//! ```
//!
//! # Overview
//!
//! This crate provides macros to derive implementations of the following traits for newtype structs:
//!
//! - Binary Arithmetic Operators: Add, BitAnd, BitOr, BitXor, Div, Mul, Rem, Sub, Shl, Shr, plus the corresponding *Assign traits.
//! - Unary Arithmetic Operators: Neg, Not.
//! - Other Operators: Deref, DerefMut, Index, IndexMut.
//! - Formatting: Binary, Debug, Display, LowerExp, LowerHex, Octal, Pointer, UpperExp, UpperHex.
//! - Miscellaneous: From.
//! - Unstable: One, Product, Sum, Zero (requires the `std-unstable` feature).
//!
//! All of these macros are named `Newtype$Trait`.
//!
//! None of these macros currently support generic newtype structs.
//!
//! ## Binary Arithmetic Operators
//!
//! Each of the binary arithmetic operators accept several deriving forms.  To use `Add` on a struct `T` as an example:
//!
//! - `NewtypeAdd`: `impl Add<T, Output=T> for T`
//! - `NewtypeAdd(&self)`: `impl<'a> Add<&'a T, Output=T> for &'a T`
//! - `NewtypeAdd(U)`: `impl Add<U, Output=T> for T`
//! - `NewtypeAdd(&self, U)`: `impl<'a> Add<U, Output=T> for &'a T`
//! - `NewtypeAdd(*)`: All four combinations of `T` and `&T`
//!
//! The `*Assign` variants accept zero or one argument only.  For example:
//!
//! - `NewtypeAddAssign`: `impl AddAssign<T> for T`
//! - `NewtypeAddAssign(&Self)`: `impl<'a> Add<&'a T> for &'a T`
//! - `NewtypeAddAssign(U)`: `impl Add<U> for T`
//! - `NewtypeAddAssign(*)`: Implements for `T` and `&T`.
//!
//! In all cases, the implementation unwraps the newtype (where necessary), forwards to the wrapped value's implementation, then re-wraps the result in the newtype.
//!
//! ## Unary Arithmetic Operators
//!
//! Each of the binary arithmetic operators accept several deriving forms.  To use `Neg` on a struct `T` as an example:
//!
//! - `NewtypeNeg`: `impl Neg<Output=T> for T`
//! - `NewtypeNeg(&self)`: `impl<'a> Neg<Output=T> for &'a T`
//! - `NewtypeNeg(*)`: both of the above
//!
//! In all cases, the implementation unwraps the newtype, forwards to the wrapped value's implementation, then re-wraps the result in the newtype.
//!
//! ## Other Operators
//!
//! `NewtypeDeref` and `NewtypeDerefMut` only support the argument-less form, and implements the corresponding trait such that the newtype structure derefs to a pointer to the wrapped value.
//!
//! `NewtypeIndex` and `NewtypeIndexMut` must be used as `NewtypeIndex(usize)`, where the argument is the type to use for indexing.  The call is forwarded to the wrapped value's implementation.
//!
//! ## Formatting
//!
//! The deriving macros for the formatting traits in [`std::fmt`][] forward to the wrapped value's implementation.
//!
//! [`std::fmt`]: http://doc.rust-lang.org/std/fmt/index.html
//!
//! ## Miscellaneous
//!
//! `NewtypeFrom` implements `std::convert::From` twice: once for converting from the wrapped type to the newtype, and once for converting from the newtype to the wrapped type.
//!
//! `NewtypeProduct` and `NewtypeSum` optionally support specifying `&Self` as an argument to generate an implementation that accepts an iterator of borrowed pointers (*e.g.* `NewtypeSum(&Self)`).
//!
//! ## Using Without `macro_attr!`
//!
//! Although designed to be used with `macro_attr!`, all of the macros in this crate can be used without it.  The following:
//!
//! ```rust
//! use macro_attr_2018::macro_attr;
//! use newtype_derive_2018::*;
//!
//! macro_attr! {
//!     #[derive(Copy, Clone, Debug, NewtypeFrom!, NewtypeAdd!, NewtypeAdd!(f32))]
//!     pub struct Meters(f32);
//! }
//! #
//! # fn main() {}
//! ```
//!
//! Can also be written as:
//!
//! ```rust
//! use newtype_derive_2018::*;
//!
//! #[derive(Copy, Clone, Debug)]
//! pub struct Meters(f32);
//!
//! NewtypeFrom! { () pub struct Meters(f32); }
//! NewtypeAdd! { () pub struct Meters(f32); }
//! NewtypeAdd! { (f32) pub struct Meters(f32); }
//! #
//! # fn main() {}
//! ```

#![no_std]

#[doc(hidden)]
pub use core::convert::From as std_convert_From;
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
        item: $vis:vis struct $name:ident($(pub)? $t:ty);
    ) => {
        $crate::as_item! {
            impl $($tr)*<$name> for $name {
                type Output = $name;
                fn $meth(self, rhs: Self) -> $name {
                    $name((self.0).$meth(rhs.0))
                }
            }
        }
    };

    (
        trait: ($($tr:tt)*)::$meth:ident,
        kind: simple_ref,
        item: $vis:vis struct $name:ident($(pub)? $t:ty);
    ) => {
        $crate::as_item! {
            impl<'a> $($tr)*<&'a $name> for &'a $name {
                type Output = $name;
                fn $meth(self, rhs: Self) -> $name {
                    $name((self.0).$meth(rhs.0))
                }
            }
        }
    };

    (
        trait: ($($tr:tt)*)::$meth:ident,
        kind: rhs_rewrap(&Self),
        item: $vis:vis struct $name:ident($(pub)? $t:ty);
    ) => {
        $crate::as_item! {
            impl<'a> $($tr)*<&'a $name> for $name {
                type Output = $name;
                fn $meth(self, rhs: &'a $name) -> $name {
                    $name((self.0).$meth(&rhs.0))
                }
            }
        }
    };

    (
        trait: ($($tr:tt)*)::$meth:ident,
        kind: rhs_rewrap($rhs:ty),
        item: $vis:vis struct $name:ident($(pub)? $t:ty);
    ) => {
        $crate::as_item! {
            impl $($tr)*<$rhs> for $name {
                type Output = $name;
                fn $meth(self, rhs: $rhs) -> $name {
                    $name((self.0).$meth(rhs))
                }
            }
        }
    };

    (
        trait: ($($tr:tt)*)::$meth:ident,
        kind: ref_rhs_rewrap(Self),
        item: $vis:vis struct $name:ident($(pub)? $t:ty);
    ) => {
        $crate::as_item! {
            impl<'a> $($tr)*<$name> for &'a $name {
                type Output = $name;
                fn $meth(self, rhs: $name) -> $name {
                    $name((self.0).$meth(rhs.0))
                }
            }
        }
    };

    (
        trait: ($($tr:tt)*)::$meth:ident,
        kind: ref_rhs_rewrap($rhs:ty),
        item: $vis:vis struct $name:ident($(pub)? $t:ty);
    ) => {
        $crate::as_item! {
            impl<'a> $($tr)*<$rhs> for &'a $name {
                type Output = $name;
                fn $meth(self, rhs: $rhs) -> $name {
                    $name((self.0).$meth(rhs))
                }
            }
        }
    };
}

#[doc(hidden)]
#[macro_export]
macro_rules! wrap_un_op {
    (
        trait: ($($tr:tt)*)::$meth:ident,
        kind: simple,
        item: $vis:vis struct $name:ident($(pub)? $t:ty);
    ) => {
        $crate::as_item! {
            impl $($tr)* for $name {
                type Output = $name;
                fn $meth(self) -> $name {
                    $name((self.0).$meth())
                }
            }
        }
    };

    (
        trait: ($($tr:tt)*)::$meth:ident,
        kind: simple_ref,
        item: $vis:vis struct $name:ident($(pub)? $t:ty);
    ) => {
        $crate::as_item! {
            impl<'a> $($tr)* for &'a $name {
                type Output = $name;
                fn $meth(self) -> $name {
                    $name((self.0).$meth())
                }
            }
        }
    };
}

#[macro_export]
macro_rules! NewtypeAdd {
    ((*) $($tts:tt)*) => {
        $crate::NewtypeAdd! { () $($tts)* }
        $crate::NewtypeAdd! { (&self) $($tts)* }
        $crate::NewtypeAdd! { (&Self) $($tts)* }
        $crate::NewtypeAdd! { (&self, Self) $($tts)* }
    };
    (() $($tts:tt)*) => {
        $crate::wrap_bin_op! { trait: ($crate::std_ops_Add)::add, kind: simple, item: $($tts)* }
    };
    ((&self) $($tts:tt)*) => {
        $crate::wrap_bin_op! { trait: ($crate::std_ops_Add)::add, kind: simple_ref, item: $($tts)* }
    };
    ((&self, $($rhs:tt)*) $($tts:tt)*) => {
        $crate::wrap_bin_op! { trait: ($crate::std_ops_Add)::add, kind: ref_rhs_rewrap($($rhs)*), item: $($tts)* }
    };
    (($($rhs:tt)*) $($tts:tt)*) => {
        $crate::wrap_bin_op! { trait: ($crate::std_ops_Add)::add, kind: rhs_rewrap($($rhs)*), item: $($tts)* }
    };
}

#[macro_export]
macro_rules! NewtypeBitAnd {
    ((*) $($tts:tt)*) => {
        $crate::NewtypeBitAnd! { () $($tts)* }
        $crate::NewtypeBitAnd! { (&self) $($tts)* }
        $crate::NewtypeBitAnd! { (&Self) $($tts)* }
        $crate::NewtypeBitAnd! { (&self, Self) $($tts)* }
    };
    (() $($tts:tt)*) => {
        $crate::wrap_bin_op! { trait: ($crate::std_ops_BitAnd)::bitand, kind: simple, item: $($tts)* }
    };
    ((&self) $($tts:tt)*) => {
        $crate::wrap_bin_op! { trait: ($crate::std_ops_BitAnd)::bitand, kind: simple_ref, item: $($tts)* }
    };
    ((&self, $($rhs:tt)*) $($tts:tt)*) => {
        $crate::wrap_bin_op! { trait: ($crate::std_ops_BitAnd)::bitand, kind: ref_rhs_rewrap($($rhs)*), item: $($tts)* }
    };
    (($($rhs:tt)*) $($tts:tt)*) => {
        $crate::wrap_bin_op! { trait: ($crate::std_ops_BitAnd)::bitand, kind: rhs_rewrap($($rhs)*), item: $($tts)* }
    };
}

#[macro_export]
macro_rules! NewtypeBitOr {
    ((*) $($tts:tt)*) => {
        $crate::NewtypeBitOr! { () $($tts)* }
        $crate::NewtypeBitOr! { (&self) $($tts)* }
        $crate::NewtypeBitOr! { (&Self) $($tts)* }
        $crate::NewtypeBitOr! { (&self, Self) $($tts)* }
    };
    (() $($tts:tt)*) => {
        $crate::wrap_bin_op! { trait: ($crate::std_ops_BitOr)::bitor, kind: simple, item: $($tts)* }
    };
    ((&self) $($tts:tt)*) => {
        $crate::wrap_bin_op! { trait: ($crate::std_ops_BitOr)::bitor, kind: simple_ref, item: $($tts)* }
    };
    ((&self, $($rhs:tt)*) $($tts:tt)*) => {
        $crate::wrap_bin_op! { trait: ($crate::std_ops_BitOr)::bitor, kind: ref_rhs_rewrap($($rhs)*), item: $($tts)* }
    };
    (($($rhs:tt)*) $($tts:tt)*) => {
        $crate::wrap_bin_op! { trait: ($crate::std_ops_BitOr)::bitor, kind: rhs_rewrap($($rhs)*), item: $($tts)* }
    };
}

#[macro_export]
macro_rules! NewtypeBitXor {
    ((*) $($tts:tt)*) => {
        $crate::NewtypeBitXor! { () $($tts)* }
        $crate::NewtypeBitXor! { (&self) $($tts)* }
        $crate::NewtypeBitXor! { (&Self) $($tts)* }
        $crate::NewtypeBitXor! { (&self, Self) $($tts)* }
    };
    (() $($tts:tt)*) => {
        $crate::wrap_bin_op! { trait: ($crate::std_ops_BitXor)::bitxor, kind: simple, item: $($tts)* }
    };
    ((&self) $($tts:tt)*) => {
        $crate::wrap_bin_op! { trait: ($crate::std_ops_BitXor)::bitxor, kind: simple_ref, item: $($tts)* }
    };
    ((&self, $($rhs:tt)*) $($tts:tt)*) => {
        $crate::wrap_bin_op! { trait: ($crate::std_ops_BitXor)::bitxor, kind: ref_rhs_rewrap($($rhs)*), item: $($tts)* }
    };
    (($($rhs:tt)*) $($tts:tt)*) => {
        $crate::wrap_bin_op! { trait: ($crate::std_ops_BitXor)::bitxor, kind: rhs_rewrap($($rhs)*), item: $($tts)* }
    };
}

#[macro_export]
macro_rules! NewtypeDiv {
    ((*) $($tts:tt)*) => {
        $crate::NewtypeDiv! { () $($tts)* }
        $crate::NewtypeDiv! { (&self) $($tts)* }
        $crate::NewtypeDiv! { (&Self) $($tts)* }
        $crate::NewtypeDiv! { (&self, Self) $($tts)* }
    };
    (() $($tts:tt)*) => {
        $crate::wrap_bin_op! { trait: ($crate::std_ops_Div)::div, kind: simple, item: $($tts)* }
    };
    ((&self) $($tts:tt)*) => {
        $crate::wrap_bin_op! { trait: ($crate::std_ops_Div)::div, kind: simple_ref, item: $($tts)* }
    };
    ((&self, $($rhs:tt)*) $($tts:tt)*) => {
        $crate::wrap_bin_op! { trait: ($crate::std_ops_Div)::div, kind: ref_rhs_rewrap($($rhs)*), item: $($tts)* }
    };
    (($($rhs:tt)*) $($tts:tt)*) => {
        $crate::wrap_bin_op! { trait: ($crate::std_ops_Div)::div, kind: rhs_rewrap($($rhs)*), item: $($tts)* }
    };
}

#[macro_export]
macro_rules! NewtypeMul {
    ((*) $($tts:tt)*) => {
        $crate::NewtypeMul! { () $($tts)* }
        $crate::NewtypeMul! { (&self) $($tts)* }
        $crate::NewtypeMul! { (&Self) $($tts)* }
        $crate::NewtypeMul! { (&self, Self) $($tts)* }
    };
    (() $($tts:tt)*) => {
        $crate::wrap_bin_op! { trait: ($crate::std_ops_Mul)::mul, kind: simple, item: $($tts)* }
    };
    ((&self) $($tts:tt)*) => {
        $crate::wrap_bin_op! { trait: ($crate::std_ops_Mul)::mul, kind: simple_ref, item: $($tts)* }
    };
    ((&self, $($rhs:tt)*) $($tts:tt)*) => {
        $crate::wrap_bin_op! { trait: ($crate::std_ops_Mul)::mul, kind: ref_rhs_rewrap($($rhs)*), item: $($tts)* }
    };
    (($($rhs:tt)*) $($tts:tt)*) => {
        $crate::wrap_bin_op! { trait: ($crate::std_ops_Mul)::mul, kind: rhs_rewrap($($rhs)*), item: $($tts)* }
    };
}

#[macro_export]
macro_rules! NewtypeRem {
    ((*) $($tts:tt)*) => {
        $crate::NewtypeRem! { () $($tts)* }
        $crate::NewtypeRem! { (&self) $($tts)* }
        $crate::NewtypeRem! { (&Self) $($tts)* }
        $crate::NewtypeRem! { (&self, Self) $($tts)* }
    };
    (() $($tts:tt)*) => {
        $crate::wrap_bin_op! { trait: ($crate::std_ops_Rem)::rem, kind: simple, item: $($tts)* }
    };
    ((&self) $($tts:tt)*) => {
        $crate::wrap_bin_op! { trait: ($crate::std_ops_Rem)::rem, kind: simple_ref, item: $($tts)* }
    };
    ((&self, $($rhs:tt)*) $($tts:tt)*) => {
        $crate::wrap_bin_op! { trait: ($crate::std_ops_Rem)::rem, kind: ref_rhs_rewrap($($rhs)*), item: $($tts)* }
    };
    (($($rhs:tt)*) $($tts:tt)*) => {
        $crate::wrap_bin_op! { trait: ($crate::std_ops_Rem)::rem, kind: rhs_rewrap($($rhs)*), item: $($tts)* }
    };
}

#[macro_export]
macro_rules! NewtypeSub {
    ((*) $($tts:tt)*) => {
        $crate::NewtypeSub! { () $($tts)* }
        $crate::NewtypeSub! { (&self) $($tts)* }
        $crate::NewtypeSub! { (&Self) $($tts)* }
        $crate::NewtypeSub! { (&self, Self) $($tts)* }
    };
    (() $($tts:tt)*) => {
        $crate::wrap_bin_op! { trait: ($crate::std_ops_Sub)::sub, kind: simple, item: $($tts)* }
    };
    ((&self) $($tts:tt)*) => {
        $crate::wrap_bin_op! { trait: ($crate::std_ops_Sub)::sub, kind: simple_ref, item: $($tts)* }
    };
    ((&self, $($rhs:tt)*) $($tts:tt)*) => {
        $crate::wrap_bin_op! { trait: ($crate::std_ops_Sub)::sub, kind: ref_rhs_rewrap($($rhs)*), item: $($tts)* }
    };
    (($($rhs:tt)*) $($tts:tt)*) => {
        $crate::wrap_bin_op! { trait: ($crate::std_ops_Sub)::sub, kind: rhs_rewrap($($rhs)*), item: $($tts)* }
    };
}

#[macro_export]
macro_rules! NewtypeShl {
    ((*) $($tts:tt)*) => {
        $crate::NewtypeShl! { () $($tts)* }
        $crate::NewtypeShl! { (&self) $($tts)* }
        $crate::NewtypeShl! { (&Self) $($tts)* }
        $crate::NewtypeShl! { (&self, Self) $($tts)* }
    };
    (() $($tts:tt)*) => {
        $crate::wrap_bin_op! { trait: ($crate::std_ops_Shl)::shl, kind: simple, item: $($tts)* }
    };
    ((&self) $($tts:tt)*) => {
        $crate::wrap_bin_op! { trait: ($crate::std_ops_Shl)::shl, kind: simple_ref, item: $($tts)* }
    };
    ((&self, $($rhs:tt)*) $($tts:tt)*) => {
        $crate::wrap_bin_op! { trait: ($crate::std_ops_Shl)::shl, kind: ref_rhs_rewrap($($rhs)*), item: $($tts)* }
    };
    (($($rhs:tt)*) $($tts:tt)*) => {
        $crate::wrap_bin_op! { trait: ($crate::std_ops_Shl)::shl, kind: rhs_rewrap($($rhs)*), item: $($tts)* }
    };
}

#[macro_export]
macro_rules! NewtypeShr {
    ((*) $($tts:tt)*) => {
        $crate::NewtypeShr! { () $($tts)* }
        $crate::NewtypeShr! { (&self) $($tts)* }
        $crate::NewtypeShr! { (&Self) $($tts)* }
        $crate::NewtypeShr! { (&self, Self) $($tts)* }
    };
    (() $($tts:tt)*) => {
        $crate::wrap_bin_op! { trait: ($crate::std_ops_Shr)::shr, kind: simple, item: $($tts)* }
    };
    ((&self) $($tts:tt)*) => {
        $crate::wrap_bin_op! { trait: ($crate::std_ops_Shr)::shr, kind: simple_ref, item: $($tts)* }
    };
    ((&self, $($rhs:tt)*) $($tts:tt)*) => {
        $crate::wrap_bin_op! { trait: ($crate::std_ops_Shr)::shr, kind: ref_rhs_rewrap($($rhs)*), item: $($tts)* }
    };
    (($($rhs:tt)*) $($tts:tt)*) => {
        $crate::wrap_bin_op! { trait: ($crate::std_ops_Shr)::shr, kind: rhs_rewrap($($rhs)*), item: $($tts)* }
    };
}

#[macro_export]
macro_rules! NewtypeNeg {
    ((*) $($tts:tt)*) => {
        $crate::NewtypeNeg! { () $($tts)* }
        $crate::NewtypeNeg! { (&self) $($tts)* }
    };
    (() $($tts:tt)*) => {
        $crate::wrap_un_op! { trait: ($crate::std_ops_Neg)::neg, kind: simple, item: $($tts)* }
    };
    ((&self) $($tts:tt)*) => {
        $crate::wrap_un_op! { trait: ($crate::std_ops_Neg)::neg, kind: simple_ref, item: $($tts)* }
    };
}

#[macro_export]
macro_rules! NewtypeNot {
    ((*) $($tts:tt)*) => {
        $crate::NewtypeNot! { () $($tts)* }
        $crate::NewtypeNot! { (&self) $($tts)* }
    };
    (() $($tts:tt)*) => {
        $crate::wrap_un_op! { trait: ($crate::std_ops_Not)::not, kind: simple, item: $($tts)* }
    };
    ((&self) $($tts:tt)*) => {
        $crate::wrap_un_op! { trait: ($crate::std_ops_Not)::not, kind: simple_ref, item: $($tts)* }
    };
}

#[macro_export]
macro_rules! NewtypeDeref {
    (() $vis:vis struct $name:ident($(pub)? $t0:ty);) => {
        impl $crate::std_ops_Deref for $name {
            type Target = $t0;

            fn deref(&self) -> &Self::Target { &self.0 }
        }
    };
}

#[macro_export]
macro_rules! NewtypeDerefMut {
    (() $vis:vis struct $name:ident($(pub)? $t0:ty);) => {
        impl $crate::std_ops_DerefMut for $name {
            fn deref_mut(&mut self) -> &mut Self::Target { &mut self.0 }
        }
    };
}

#[macro_export]
macro_rules! NewtypeIndex {
    (($Index:ty) $vis:vis struct $name:ident($(pub)? $t0:ty);) => {
        impl $crate::std_ops_Index<$Index> for $name {
            type Output = <$t0 as $crate::std_ops_Index<$Index>>::Output;

            fn index(&self, index: $Index) -> &Self::Output {
                <$t0 as $crate::std_ops_Index<$Index>>::index(&self.0, index)
            }
        }
    };
}

#[macro_export]
macro_rules! NewtypeIndexMut {
    (($Index:ty) $vis:vis struct $name:ident($(pub)? $t0:ty);) => {
        impl $crate::std_ops_IndexMut<$Index> for $name {
            fn index_mut(&mut self, index: $Index) -> &mut Self::Output {
                <$t0 as $crate::std_ops_IndexMut<$Index>>::index_mut(&mut self.0, index)
            }
        }
    };
}

#[macro_export]
macro_rules! NewtypeFrom {
    (() $vis:vis struct $name:ident($(pub)? $t0:ty);) => {
        impl $crate::std_convert_From<$t0> for $name {
            fn from(v: $t0) -> Self { $name(v) }
        }

        impl $crate::std_convert_From<$name> for $t0 {
            fn from(v: $name) -> Self { v.0 }
        }
    };
}


#[macro_export]
macro_rules! NewtypeBinary {
    (() $vis:vis struct $name:ident($(pub)? $t0:ty);) => {
        impl $crate::std_fmt_Binary for $name {
            fn fmt(&self, fmt: &mut $crate::std_fmt_Formatter) -> $crate::std_fmt_Result {
                $crate::std_fmt_Binary::fmt(&self.0, fmt)
            }
        }
    };
}

#[macro_export]
macro_rules! NewtypeDebug {
    (() $vis:vis struct $name:ident($(pub)? $t0:ty);) => {
        impl $crate::std_fmt_Debug for $name {
            fn fmt(&self, fmt: &mut $crate::std_fmt_Formatter) -> $crate::std_fmt_Result {
                $crate::std_fmt_Debug::fmt(&self.0, fmt)
            }
        }
    };
}

#[macro_export]
macro_rules! NewtypeDisplay {
    (() $vis:vis struct $name:ident($(pub)? $t0:ty);) => {
        impl $crate::std_fmt_Display for $name {
            fn fmt(&self, fmt: &mut $crate::std_fmt_Formatter) -> $crate::std_fmt_Result {
                $crate::std_fmt_Display::fmt(&self.0, fmt)
            }
        }
    };
}

#[macro_export]
macro_rules! NewtypeLowerExp {
    (() $vis:vis struct $name:ident($(pub)? $t0:ty);) => {
        impl $crate::std_fmt_LowerExp for $name {
            fn fmt(&self, fmt: &mut $crate::std_fmt_Formatter) -> $crate::std_fmt_Result {
                $crate::std_fmt_LowerExp::fmt(&self.0, fmt)
            }
        }
    };
}

#[macro_export]
macro_rules! NewtypeLowerHex {
    (() $vis:vis struct $name:ident($(pub)? $t0:ty);) => {
        impl $crate::std_fmt_LowerHex for $name {
            fn fmt(&self, fmt: &mut $crate::std_fmt_Formatter) -> $crate::std_fmt_Result {
                $crate::std_fmt_LowerHex::fmt(&self.0, fmt)
            }
        }
    };
}

#[macro_export]
macro_rules! NewtypeOctal {
    (() $vis:vis struct $name:ident($(pub)? $t0:ty);) => {
        impl $crate::std_fmt_Octal for $name {
            fn fmt(&self, fmt: &mut $crate::std_fmt_Formatter) -> $crate::std_fmt_Result {
                $crate::std_fmt_Octal::fmt(&self.0, fmt)
            }
        }
    };
}

#[macro_export]
macro_rules! NewtypePointer {
    (() $vis:vis struct $name:ident($(pub)? $t0:ty);) => {
        impl $crate::std_fmt_Pointer for $name {
            fn fmt(&self, fmt: &mut $crate::std_fmt_Formatter) -> $crate::std_fmt_Result {
                $crate::std_fmt_Pointer::fmt(&self.0, fmt)
            }
        }
    };
}

#[macro_export]
macro_rules! NewtypeUpperExp {
    (() $vis:vis struct $name:ident($(pub)? $t0:ty);) => {
        impl $crate::std_fmt_UpperExp for $name {
            fn fmt(&self, fmt: &mut $crate::std_fmt_Formatter) -> $crate::std_fmt_Result {
                $crate::std_fmt_UpperExp::fmt(&self.0, fmt)
            }
        }
    };
}

#[macro_export]
macro_rules! NewtypeUpperHex {
    (() $vis:vis struct $name:ident($(pub)? $t0:ty);) => {
        impl $crate::std_fmt_UpperHex for $name {
            fn fmt(&self, fmt: &mut $crate::std_fmt_Formatter) -> $crate::std_fmt_Result {
                $crate::std_fmt_UpperHex::fmt(&self.0, fmt)
            }
        }
    };
}
