//! scm1 --- wasm bindings
use scm1::{E,run};use String as S;
use wasm_bindgen::prelude::*;
#[cfg(feature = "wee")] #[global_allocator] static ALLOC: wee_alloc::WeeAlloc = wee_alloc::WeeAlloc::INIT;
#[wasm_bindgen] extern {fn alert(s: &str);}
pub fn set_panic_hook() {#[cfg(feature = "panic")] console_error_panic_hook::set_once()}
#[wasm_bindgen] pub fn rep(i:&str) -> S {run(i,&mut E::n(None))}
#[wasm_bindgen] pub fn m() {alert("greetings, stranger")}
pub fn main() {#[cfg(feature="panic")] console_error_panic_hook::set_once();m()}
