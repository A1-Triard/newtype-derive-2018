// Copyright (c) 2015 macro-attr contributors.
// Copyright (c) 2021 Warlock <internalmike@gmail.com>
//
// Licensed under the MIT license (see LICENSE or <http://opensource.org
// /licenses/MIT>) or the Apache License, Version 2.0 (see LICENSE of
// <http://www.apache.org/licenses/LICENSE-2.0>), at your option. All
// files in the project carrying such notice may not be copied, modified,
// or distributed except according to those terms.

use macro_attr_2018::macro_attr;
use newtype_derive_2018::*;

macro_attr! {
    #[derive(
        Clone, Eq, PartialEq, Debug,
        NewtypeDeref!, NewtypeDerefMut!,
        NewtypeIndex!(usize), NewtypeIndexMut!(usize)
    )]
    pub struct Dummy(Vec<i32>);
}
