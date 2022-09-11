use scm1::{R,E,parse,tok,eval,repl,ld};
static LIB:&str="SCM1_LIB";

fn main() -> R<()> {
  let mut e=E::n(None);let e = e.init();let mut cc = false;
  if let Ok(x) = std::env::var(LIB) {ld(x,e,cc)?};
  let a:Vec<String>=std::env::args().skip(1).collect();for i in &a {
    if i.starts_with('-') { for x in i.chars().skip(1) { match x {
      'h' => println!(include_str!("h")),'i' => cc=true, y => eprintln!("invalid opt {}", y)}}}
    else if i.starts_with('"') {
      println!("  {}", eval(&parse(&tok(i.trim_matches('"').to_string()))?.0,e)? );} else {
      ld(i,e,false)?}}; if cc || a.is_empty() {repl(e)?} Ok(())}
