// Copyright (c) 2015 macro-attr contributors.
// Copyright (c) 2021 Warlock <internalmike@gmail.com>
//
// Licensed under the MIT license (see LICENSE or <http://opensource.org
// /licenses/MIT>) or the Apache License, Version 2.0 (see LICENSE of
// <http://www.apache.org/licenses/LICENSE-2.0>), at your option. All
// files in the project carrying such notice may not be copied, modified,
// or distributed except according to those terms.

#![recursion_limit = "128"]

use macro_attr_2018::macro_attr;
use newtype_derive_2018::*;

use std::fmt::{self, Binary, Debug, Display, LowerExp, LowerHex, Octal, Pointer, UpperExp, UpperHex};

macro_rules! impl_fmt {
    (impl $tr:ident for $name:ident: $msg:expr) => {
        impl $tr for $name {
            fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
                write!(fmt, $msg)
            }
        }
    };
}

macro_attr! {
    #[derive(
        Copy, Clone, Eq, PartialEq, Debug,
        NewtypeAdd!, NewtypeAdd!(&self), NewtypeAdd!(i32), NewtypeAdd!(&self, i32),
        NewtypeBitAnd!, NewtypeBitAnd!(&self),
        NewtypeBitOr!, NewtypeBitOr!(&self),
        NewtypeBitXor!, NewtypeBitXor!(&self),
        NewtypeDiv!, NewtypeDiv!(&self),
        NewtypeMul!, NewtypeMul!(&self),
        NewtypeRem!, NewtypeRem!(&self),
        NewtypeSub!, NewtypeSub!(&self),
        NewtypeShl!(), NewtypeShl!(&self), NewtypeShl!(usize), NewtypeShl!(&self, usize),
        NewtypeShr!(), NewtypeShr!(&self), NewtypeShr!(usize), NewtypeShr!(&self, usize),
        NewtypeNeg!, NewtypeNeg!(&self),
        NewtypeNot!, NewtypeNot!(&self),
    )]
    pub struct Dummy1(pub i32);
}

macro_attr! {
    #[derive(
        Clone, Eq, PartialEq, Debug,
        NewtypeDeref!, NewtypeDerefMut!,
        NewtypeIndex!(usize), NewtypeIndexMut!(usize)
    )]
    pub struct Dummy2(Vec<i32>);
}

struct Dummy3Inner;

impl_fmt!(impl Binary for Dummy3Inner: "binary");
impl_fmt!(impl Debug for Dummy3Inner: "debug");
impl_fmt!(impl Display for Dummy3Inner: "display");
impl_fmt!(impl LowerExp for Dummy3Inner: "lowerexp");
impl_fmt!(impl LowerHex for Dummy3Inner: "lowerhex");
impl_fmt!(impl Octal for Dummy3Inner: "octal");
impl_fmt!(impl Pointer for Dummy3Inner: "pointer");
impl_fmt!(impl UpperExp for Dummy3Inner: "upperexp");
impl_fmt!(impl UpperHex for Dummy3Inner: "upperhex");

macro_attr! {
    #[derive(
        NewtypeBinary!,
        NewtypeDebug!,
        NewtypeDisplay!,
        NewtypeLowerExp!,
        NewtypeLowerHex!,
        NewtypeOctal!,
        NewtypePointer!,
        NewtypeUpperExp!,
        NewtypeUpperHex!
    )]
    struct Dummy3(Dummy3Inner);
}

#[test]
fn test_pub_interior_fields() {
    let _ = Dummy1(0);
    let _ = Dummy2(vec![0]);
    let _ = Dummy3(Dummy3Inner);
}
