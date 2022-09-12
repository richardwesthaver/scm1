use scm1::{R,E,parse,tok,eval,repl,ld,H,LIB};
fn main() -> R<()> {
  let mut e=E::n(None).init();let mut cc = false;
  if let Ok(x) = std::env::var(LIB) {ld(x,&mut e,cc)?};
  let a:Vec<String>=std::env::args().skip(1).collect();for i in &a {
    if i.starts_with('-') { for x in i.chars().skip(1) { match x {
      'h' => println!("{}",H),'i' => cc=true, y => eprintln!("invalid opt {}", y)}}} else {
      match ld(i,&mut e,false) {Ok(x)=>x,Err(_)=> println!("  {}", eval(&parse(&tok(i.to_string()))?.0,&mut e)?)}}}
  if cc || a.is_empty() {repl(&mut e)} else {Ok(())}}
