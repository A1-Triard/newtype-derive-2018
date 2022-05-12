#![feature(default_alloc_error_handler)]
#![feature(start)]

#![deny(warnings)]

#![no_std]

use core::alloc::Layout;
use core::panic::PanicInfo;
use newtype_derive_2018::NewtypeAdd;
use macro_attr_2018::macro_attr;
#[cfg(not(windows))]
use libc::exit;
use libc_alloc::LibcAlloc;
#[cfg(windows)]
use winapi::shared::minwindef::UINT;
#[cfg(windows)]
use winapi::um::processthreadsapi::ExitProcess;

#[cfg(windows)]
#[link(name="msvcrt")]
extern { }

#[global_allocator]
static ALLOCATOR: LibcAlloc = LibcAlloc;

#[cfg(windows)]
unsafe fn exit(code: UINT) -> ! {
    ExitProcess(code);
    loop { }
}

#[panic_handler]
pub extern fn panic(_info: &PanicInfo) -> ! {
    unsafe { exit(99) }
}

#[no_mangle]
pub fn rust_oom(_layout: Layout) -> ! {
    unsafe { exit(98) }
}

macro_attr! {
    #[derive(NewtypeAdd!)]
    pub struct Happy(pub i32);
}

#[start]
pub fn main(_argc: isize, _argv: *const *const u8) -> isize {
    let a = Happy(6);
    let b = Happy(7);
    let c = a + b;
    let d: i32 = c.0;
    assert_eq!(d, 13);
    0
}
