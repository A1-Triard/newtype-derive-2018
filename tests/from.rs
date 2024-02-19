// Copyright (c) 2015 macro-attr contributors.
// Copyright (c) 2021 Warlock <internalmike@gmail.com>
//
// Licensed under the MIT license (see LICENSE or <http://opensource.org
// /licenses/MIT>) or the Apache License, Version 2.0 (see LICENSE of
// <http://www.apache.org/licenses/LICENSE-2.0>), at your option. All
// files in the project carrying such notice may not be copied, modified,
// or distributed except according to those terms.

#![deny(warnings)]

use macro_attr_2018::macro_attr;
use newtype_derive_2018::*;

macro_attr! {
    #[derive(
        Clone, Eq, PartialEq, Debug,
        NewtypeFrom!,
    )]
    pub struct WrappedI32(i32);
}

macro_attr! {
    #[derive(
        Clone, Eq, PartialEq, Debug,
        // NOTE: "unwrap" impl is forbidden by orphan rule
        NewtypeFrom!(wrap),
    )]
    pub struct Wrapper<T>(T);
}

macro_attr! {
    #[derive(
        Clone, Eq, PartialEq, Debug,
        NewtypeFrom!(unwrap),
    )]
    pub struct Not42(i32);
}

impl TryFrom<i32> for Not42 {
    type Error = ();

    fn try_from(value: i32) -> Result<Self, Self::Error> {
        if value == 42 {
            Err(())
        } else {
            Ok(Self(value))
        }
    }
}

#[test]
fn concrete() {
    assert_eq!(WrappedI32(42), 42.into());
    assert_eq!(42, WrappedI32(42).into());
}

#[test]
fn generic() {
    assert_eq!(Wrapper(true), true.into());
    
    // Will not compile.
    // TODO: use `compiletest_rs` crate?
    // assert_eq!(true, Wrapper(true).into());
}

#[test]
fn validated() {
    assert_eq!(24, Not42(24).into());

    // Will not compile.
    // TODO: use `compiletest_rs` crate?
    // assert_eq!(Not42(24), 24.into());

    assert_eq!(Ok(Not42(24)), 24.try_into());
}
