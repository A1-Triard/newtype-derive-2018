// Copyright (c) 2015 macro-attr contributors.
// Copyright (c) 2021 Warlock <internalmike@gmail.com>
//
// Licensed under the MIT license (see LICENSE or <http://opensource.org
// /licenses/MIT>) or the Apache License, Version 2.0 (see LICENSE of
// <http://www.apache.org/licenses/LICENSE-2.0>), at your option. All
// files in the project carrying such notice may not be copied, modified,
// or distributed except according to those terms.

#![deny(warnings)]
#![recursion_limit = "128"]

use macro_attr_2018::macro_attr;
use newtype_derive_2018::*;
use std::ops::{Add, BitAnd, BitOr, BitXor, Div, Mul, Rem, Sub, Shl, Shr, Neg, Not};

macro_attr! {
    #[derive(
        Copy, Clone, Eq, PartialEq, Debug,
        NewtypeAdd!(where T: Add<Output=T>),
        NewtypeAdd!((&self) where T: Add<Output=T>),
        NewtypeAdd!((T) where T: Add<Output=T>),
        NewtypeAdd!((&self, T) where T: Add<Output=T>),
        NewtypeBitAnd!(where T: BitAnd<Output=T>),
        NewtypeBitAnd!((&self) where T: BitAnd<Output=T>),
        NewtypeBitOr!(where T: BitOr<Output=T>),
        NewtypeBitOr!((&self) where T: BitOr<Output=T>),
        NewtypeBitXor!(where T: BitXor<Output=T>),
        NewtypeBitXor!((&self) where T: BitXor<Output=T>),
        NewtypeDiv!(where T: Div<Output=T>),
        NewtypeDiv!((&self) where T: Div<Output=T>),
        NewtypeMul!(where T: Mul<Output=T>),
        NewtypeMul!((&self) where T: Mul<Output=T>),
        NewtypeRem!(where T: Rem<Output=T>),
        NewtypeRem!((&self) where T: Rem<Output=T>),
        NewtypeSub!(where T: Sub<Output=T>),
        NewtypeSub!((&self) where T: Sub<Output=T>),
        NewtypeShl!(where T: Shl<Output=T>),
        NewtypeShl!((&self) where T: Shl<Output=T>),
        NewtypeShl!((usize) where T: Shl<usize, Output=T>),
        NewtypeShl!((&self, usize) where T: Shl<usize, Output=T>),
        NewtypeShr!(where T: Shr<Output=T>),
        NewtypeShr!((&self) where T: Shr<Output=T>),
        NewtypeShr!((usize) where T: Shr<usize, Output=T>),
        NewtypeShr!((&self, usize) where T: Shr<usize, Output=T>),
        NewtypeNeg!(where T: Neg<Output=T>),
        NewtypeNeg!((&self) where T: Neg<Output=T>),
        NewtypeNot!(where T: Not<Output=T>),
        NewtypeNot!((&self) where T: Not<Output=T>),
    )]
    pub struct Dummy<T: Copy>(T);
}

#[test]
fn test_arith_i16() {
    let a = Dummy(4i16);
    let b = Dummy(7i16);

    assert_eq!(a + b, Dummy(4i16 + 7));
    assert_eq!(&a + &b, Dummy(4i16 + 7));
    assert_eq!(a + 7, Dummy(4i16 + 7));
    assert_eq!(&a + 7, Dummy(4i16 + 7));
    assert_eq!(a & b, Dummy(4i16 & 7));
    assert_eq!(&a & &b, Dummy(4i16 & 7));
    assert_eq!(a | b, Dummy(4i16 | 7));
    assert_eq!(&a | &b, Dummy(4i16 | 7));
    assert_eq!(a ^ b, Dummy(4i16 ^ 7));
    assert_eq!(&a ^ &b, Dummy(4i16 ^ 7));
    assert_eq!(a / b, Dummy(4i16 / 7));
    assert_eq!(&a / &b, Dummy(4i16 / 7));
    assert_eq!(a * b, Dummy(4i16 * 7));
    assert_eq!(&a * &b, Dummy(4i16 * 7));
    assert_eq!(a % b, Dummy(4i16 % 7));
    assert_eq!(&a % &b, Dummy(4i16 % 7));
    assert_eq!(a - b, Dummy(4i16 - 7));
    assert_eq!(&a - &b, Dummy(4i16 - 7));

    assert_eq!(a << b, Dummy(4 << 7));
    assert_eq!(&a << &b, Dummy(4 << 7));
    assert_eq!(a << 7, Dummy(4 << 7));

    assert_eq!(a >> b, Dummy(4 >> 7));
    assert_eq!(&a >> &b, Dummy(4 >> 7));
    assert_eq!(a >> 7, Dummy(4 >> 7));

    assert_eq!(-a, Dummy(-4));
    assert_eq!(-&a, Dummy(-4));
    assert_eq!(!a, Dummy(!4));
    assert_eq!(!&a, Dummy(!4));
}

#[test]
fn test_arith_u8() {
    let a = Dummy(4u8);
    let b = Dummy(7u8);

    assert_eq!(a + b, Dummy(4 + 7));
    assert_eq!(&a + &b, Dummy(4 + 7));
    assert_eq!(a + 7, Dummy(4 + 7));
    assert_eq!(&a + 7, Dummy(4 + 7));
    assert_eq!(a & b, Dummy(4 & 7));
    assert_eq!(&a & &b, Dummy(4 & 7));
    assert_eq!(a | b, Dummy(4 | 7));
    assert_eq!(&a | &b, Dummy(4 | 7));
    assert_eq!(a ^ b, Dummy(4 ^ 7));
    assert_eq!(&a ^ &b, Dummy(4 ^ 7));
    assert_eq!(a / b, Dummy(4 / 7));
    assert_eq!(&a / &b, Dummy(4 / 7));
    assert_eq!(a * b, Dummy(4 * 7));
    assert_eq!(&a * &b, Dummy(4 * 7));
    assert_eq!(a % b, Dummy(4 % 7));
    assert_eq!(&a % &b, Dummy(4 % 7));
    assert_eq!(b - a, Dummy(7 - 4));
    assert_eq!(&b - &a, Dummy(7 - 4));

    assert_eq!(a << b, Dummy(4 << 7));
    assert_eq!(&a << &b, Dummy(4 << 7));
    assert_eq!(a << 7, Dummy(4 << 7));

    assert_eq!(a >> b, Dummy(4 >> 7));
    assert_eq!(&a >> &b, Dummy(4 >> 7));
    assert_eq!(a >> 7, Dummy(4 >> 7));

    assert_eq!(!a, Dummy(!4));
    assert_eq!(!&a, Dummy(!4));
}
