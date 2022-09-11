//! scm1w --- scm1 wasm
use scm1::{E,run};use {String as S,Result as R};
use wasm_bindgen::{JsCast, JsValue as J, prelude::*};
use xterm_js_rs::{OnKeyEvent, Terminal as T, TerminalOptions as TO, Theme, addons::fit::FitAddon};
#[cfg(feature = "wee")] #[global_allocator] static ALLOC: wee_alloc::WeeAlloc = wee_alloc::WeeAlloc::INIT;
#[wasm_bindgen] extern {fn alert(s: &str);}
pub fn set_panic_hook() {#[cfg(feature = "panic")] console_error_panic_hook::set_once()}

const KEY_ENTER: u32 = 13;const KEY_BACKSPACE: u32 = 8;
const KEY_LEFT_ARROW: u32 = 37;const KEY_RIGHT_ARROW: u32 = 39;
const KEY_C: u32 = 67;const KEY_L: u32 = 76;
const CURSOR_LEFT: &str = "\x1b[D";const CURSOR_RIGHT: &str = "\x1b[C";
const PROMPT: &str = "λ ";
fn prompt(term: &T) {term.writeln("");term.write(PROMPT);}
#[wasm_bindgen] pub fn rep(i:&str) -> S {run(i,&mut E::n(None))}
#[wasm_bindgen(start)] pub fn main() -> R<(),J> {
  #[cfg(feature="panic")] console_error_panic_hook::set_once();
  let t1: T = T::new(
    TO::new().with_rows(50).with_cursor_blink(true).with_cursor_width(10)
      .with_font_size(20).with_draw_bold_text_in_bright_colors(true).with_right_click_selects_word(true).with_theme(
        Theme::new().with_foreground("#98FB98").with_background("#000000")));
  let e = web_sys::window().unwrap().document().unwrap().get_element_by_id("term").unwrap();
  t1.writeln("Supported keys in this example: <Printable-Characters> <Enter> <Backspace> <Left-Arrow> <Right-Arrow> <Ctrl-C> <Ctrl-L>");
  t1.open(e.dyn_into()?);
  prompt(&t1);  
  let mut i = S::new();let mut c_c = 0;
  let t:T = t1.clone().dyn_into()?;
  let cb = Closure::wrap(Box::new(move |x:OnKeyEvent| {
    let ev = x.dom_event();
    match ev.key_code() {
      KEY_ENTER => {if !i.is_empty() {t.writeln("");t.writeln(&format!("{} '{}'", i.len(), i));i.clear();c_c=0;} prompt(&t);}
      KEY_BACKSPACE => {if c_c > 0 {t.write("\u{0008} \u{0008}");i.pop();c_c -= 1;}}
      KEY_LEFT_ARROW => {if c_c > 0 {t.write(CURSOR_LEFT);c_c -= 1;}}
      KEY_RIGHT_ARROW => {if c_c < i.len() {t.write(CURSOR_RIGHT);c_c += 1;}}
      KEY_L if ev.ctrl_key() => t.clear(),
      KEY_C if ev.ctrl_key() => {prompt(&t);i.clear();c_c = 0;}
      _ => {if !ev.alt_key() && !ev.alt_key() && !ev.ctrl_key() && !ev.meta_key() {
        t.write(&ev.key());i.push_str(&x.key());c_c += 1;}}}}) as Box<dyn FnMut(_)>);

  t1.on_key(cb.as_ref().unchecked_ref());cb.forget();
  let ad = FitAddon::new();t1.load_addon(ad.clone().dyn_into::<FitAddon>()?.into());
  ad.fit();t1.focus();Ok(())}
