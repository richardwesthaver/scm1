//! scm1w --- scm1 wasm
use scm1::{E,run,rlw,P};use {String as S,Result as R,Box as B};use wasm_bindgen::{JsCast, JsValue as J, prelude::*};
use xterm_js_rs::{OnKeyEvent as OE, Terminal as T, TerminalOptions as TO, Theme as TH, addons::fit::FitAddon as FA};
#[cfg(feature = "wee")] #[global_allocator] static ALLOC: wee_alloc::WeeAlloc = wee_alloc::WeeAlloc::INIT;
#[wasm_bindgen] extern {fn alert(s: &str);}
pub fn spook() {#[cfg(feature = "panic")] console_error_panic_hook::set_once()}
const ENT: u32 = 13;const DEL: u32 = 8;const C: u32 = 67;const L: u32 = 76;
const AL: u32 = 37;const AR: u32 = 39;const CL: &str = "\x1b[D";const CR: &str = "\x1b[C";
fn prompt(term: &T) {term.writeln("");term.write(P);}
#[wasm_bindgen(start)] pub fn main() -> R<(),J> {
  spook(); let t1: T = T::new(
    TO::new().with_rows(50).with_cursor_blink(true).with_cursor_width(10)
      .with_font_size(20).with_draw_bold_text_in_bright_colors(true).with_right_click_selects_word(true).with_theme(
        TH::new().with_foreground("#98FB98").with_background("#000000")));
  let e = web_sys::window().unwrap().document().unwrap().get_element_by_id("term").unwrap();
  t1.writeln(&rlw());
  t1.open(e.dyn_into()?);prompt(&t1);  
  let mut i = S::new();let mut c_c = 0;let t:T = t1.clone().dyn_into()?;let mut e = E::n(None).init();
  let cb = Closure::wrap(B::new(move |x:OE| { let ev = x.dom_event();match ev.key_code() {
    ENT => {if !i.is_empty() {t.writeln("");t.writeln(&format!("{}", run(&i,&mut e)));i.clear();c_c=0;} prompt(&t);}
    DEL => {if c_c > 0 {t.write("\u{0008} \u{0008}");i.pop();c_c -= 1;}}
    AL => {if c_c > 0 {t.write(CL);c_c -= 1;}}
    AR => {if c_c < i.len() {t.write(CR);c_c += 1;}}
    L if ev.ctrl_key() => t.clear(),C if ev.ctrl_key() => {prompt(&t);i.clear();c_c = 0;}
    _ => {if !ev.alt_key() && !ev.alt_key() && !ev.ctrl_key() && !ev.meta_key() {
      t.write(&ev.key());i.push_str(&x.key());c_c += 1;}}}}) as B<dyn FnMut(_)>);
  t1.on_key(cb.as_ref().unchecked_ref());cb.forget();
  let fa = FA::new(); t1.load_addon(fa.clone().dyn_into::<FA>()?.into()); fa.fit();t1.focus();Ok(())}
