// Copyright (c) 2015 macro-attr contributors.
// Copyright (c) 2021 Warlock <internalmike@gmail.com>
//
// Licensed under the MIT license (see LICENSE or <http://opensource.org
// /licenses/MIT>) or the Apache License, Version 2.0 (see LICENSE of
// <http://www.apache.org/licenses/LICENSE-2.0>), at your option. All
// files in the project carrying such notice may not be copied, modified,
// or distributed except according to those terms.

#[macro_use] extern crate macro_attr_2018;
#[macro_use] extern crate newtype_derive_2018;

macro_attr! {
    #[derive(Copy, Clone, Eq, PartialEq, Debug,
        NewtypeAdd!(*),
        NewtypeNeg!(*),
        )]
    pub struct Dummy(i32);
}

macro_attr! {
    #[derive(Copy, Clone, Eq, PartialEq, Debug,
        NewtypeAdd!(*),
        NewtypeNeg!(*),
        )]
    pub struct DummyPub(pub i32);
}

#[test]
fn test_arith() {
    let a = Dummy(4);
    let b = Dummy(7);
    let c = Dummy(11);

    assert_eq!(a + b, c);
    assert_eq!(&a + b, c);
    assert_eq!(a + &b, c);
    assert_eq!(&a + &b, c);

    assert_eq!(-a, Dummy(-4));
    assert_eq!(-&a, Dummy(-4));

    let _ = DummyPub(0);
}
