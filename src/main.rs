//! scm1 --- a no-fluff tree-walking toy scheme
use {String as S, Vec as V,std::{option::Option as O, collections::HashMap as M, error::Error, rc::Rc,path::Path,fs::{File as F},
	   io::{BufReader,BufRead,stdin,stdout,Write},env::args,fmt::{Display, Debug, Result as FR, Formatter as FF}}};
#[derive(Debug, Clone)] struct Er(S); type R<X> = Result<X,Er>;
impl Er {fn n(i:&str) -> Er {Er(i.to_string())}} impl Error for Er {}
impl Display for Er {fn fmt(&self, f: &mut FF<'_>) -> FR {f.write_str(&self.0)}}
macro_rules! er {($s:expr)=>{Er::n($s)}} macro_rules! err {($s:expr)=>{Err(er!($s))}}
#[derive(Clone)] enum A {A(S),B(bool),N(f64),S(S),F(fn(&[A])->R<A>),G(G),L(V<A>)}
#[derive(Clone)] struct G{args:Rc<A>,body:Rc<A>} impl G { fn n(args:Rc<A>,body:Rc<A>)->G {G{args,body}}}
impl PartialEq for A {fn eq(&self, rhs:&A) -> bool {match &self {
  A::F(_)|A::G(_) => false,A::L(x) => match &rhs {A::L(y) => { for (idx,yx) in y.iter().enumerate() { 
    match x[idx].eq(yx) {true => continue, false => return false}} true},_ => false} _ => false,}}} // TODO
impl Debug for A { fn fmt(&self, f: &mut FF<'_>) -> FR { use crate::A::*; match &self {
  A(_)|B(_)|N(_)|S(_)|L(_) => {f.write_str(&format!("{:?}",self))},F(_)|G(_) => {f.write_str("#fn")}}}}
impl Display for A { fn fmt(&self, f: &mut FF<'_>) -> FR {write!(f,"{}",match self {
  A::A(x) => x.clone(),A::S(x) => x.trim_matches('"').to_string().clone(),A::F(_)|A::G(_) => "#fn".to_string(),
  A::B(x) => {let r = if *x {"#t"} else {"#f"}; r.to_string()},A::N(x) => x.to_string(),
  A::L(x) => {let xs: V<S> = x.iter().map(|x| x.to_string()).collect();format!("{}", xs.join(" "))}})}}
macro_rules! af {($a:ident,$f:expr)=>{A::F(|$a:&[A]|->R<A>{$f})}}macro_rules! ab {($f:expr) => {{|i: &[A]| -> R<A> {
  let ln = parseln(i)?;let (car,cdr) = (ln.first().ok_or(er!("expected at least one number"))?, &ln[1..]);
  fn f(far:&f64,fdr:&[f64])-> bool {match fdr.first() {Some(x) => $f(far,x) && f(x,&fdr[1..]),None => true}}
  Ok(A::B(f(car,cdr)))}}}}
#[derive(Default,Clone)] struct E<'e> {env:M<S,A>,out:O<&'e E<'e>>} impl<'e> E<'e> {
  fn new(out:O<&'e E<'e>>) -> E<'e> {E{env:M::new(),out}}
  fn get(&self,k:&str) -> O<&A> {
    match self.env.get(k) {Some(x) => Some(x),None => match &self.out { Some(o) => o.get(k),None=>None}}}
  fn set(&mut self,k:&str,v:A) -> O<A> {self.env.insert(k.to_string(),v)}
  fn init(&'e mut self) -> &'e mut E<'e> {
    [('$',af!(x,Ok(A::S(format!("\"{}\"", (x.into_iter().map(|x| x.to_string()).collect::<Vec<S>>().join(" "))))))),
     ('!',A::F(ab!(|a,b|a!=b))),('=',A::F(ab!(|a,b|a==b))),('>',A::F(ab!(|a,b|a>b))),('<',A::F(ab!(|a,b|a<b))),
     (',',af!(x,Ok(A::L(x.to_vec())))),
     ('+',af!(x,Ok(A::N(parseln(x)?.iter().fold(0.0, |r,a| r+a))))),('-',af!(x,Ok(A::N(parseln(x)?.iter().fold(0.0, |r,a| r-a))))),
     ('%',af!(x,Ok(A::N(parseln(x)?.into_iter().reduce(|r,a| r%a).unwrap())))),
     ('/',af!(x,Ok(A::N(parseln(x)?.into_iter().reduce(|r,a| r/a).unwrap())))),
     ('^',af!(x,Ok(A::N(parseln(x)?.into_iter().reduce(|r,a| r.powf(a)).unwrap())))),
     ('*',af!(x,{let r = parseln(x)?.iter().product(); Ok(A::N(r))}))].map(|(x,y)| {self.env.insert(x.to_string(),y)});self}}
fn tok(i:S) -> V<S> {let i = if let Some(n) = i.find(';') {&i[..n]} else {&i};if i.len() == 0 { return vec![] } else {
  let mut r = vec![];if i.starts_with('"') {
    let ms: V<&str> = i.splitn(2, '"').collect();if ms.len() == 0 { return r } else {r.push(format!("\"{}", ms[1]))}} //lol
  let i = i.replace("(", " ( ").replace(")", " ) ");
  let i:V<S> = i.split_whitespace().map(|x| x.to_string()).collect();r.extend_from_slice(&i);r}}
fn parseln(i:&[A]) -> R<V<f64>> {i.iter().map(|x| parsen(x)).collect()}
fn parsels(i:Rc<A>) -> R<V<S>> {
  let ls = match i.as_ref() {A::L(x) => Ok(x.clone()),_ => err!("expected args form to be a list")}?;
  ls.iter().map(|x| match x {A::A(l) => Ok(l.clone()),_ => err!("expected symbols in arg list")}).collect()}
fn parsen(n:&A) -> R<f64> {match n {A::N(n) => Ok(*n), _ => err!("expected number")}}
fn parse<'a>(i:&'a[S]) -> R<(A,&'a[S])> {let (car, cdr) = i.split_first().ok_or(er!("could not get token"))?;
  match &car[..] {"(" => seq(cdr),")" => err!("unexpected `)`"),_ => Ok((atom(car), cdr))}}
fn seq<'a>(i:&'a [S]) -> R<(A,&'a [S])> {let mut res = vec![]; let mut xs = i;loop {
  let (car, cdr) = xs.split_first().ok_or(er!("missing closing `)`"))?;
  if car == ")" {return Ok((A::L(res), cdr))} let (nx,ni) = parse(&xs)?;res.push(nx); xs = ni;}}
fn atom(i:&str) -> A {match i.parse().ok() {Some(n) => A::N(n),None =>{
  if i.starts_with('"') {A::S(i.to_string().clone())}
  else if i.eq("#t") {A::B(true)} else if i.eq("#f") {A::B(false)} else {A::A(i.to_string().clone())}}}}
fn eval(p:&A,e:&mut E) -> R<A> {match p {
  A::A(x) => e.get(&x).ok_or(er!(&format!("unexpected symbol `{}`", x))).map(|x| x.clone()),
  A::B(_)|A::N(_)|A::S(_) => Ok(p.clone()),A::L(x) => {
    let (car,cdr) = (x.first().ok_or(er!("expected non-empty list"))?, &x[1..]);match evala(car,cdr,e) {
      Some(r) => r,None => {let fx = eval(car,e)?;match fx {
	A::F(f) => {let rx = cdr.iter().map(|x| eval(x,e)).collect::<R<V<A>>>();f(&rx?)},
	A::G(g) => {let fe = &mut fne(g.args,cdr,e)?;eval(&g.body,fe)},
	_ => err!("first form must be a function")}}}},_ => err!("unexpected form")}}
fn evala(car:&A,cdr:&[A],e:&mut E) -> O<R<A>> {match car {A::A(x) => match x.as_ref() {
  "?" => Some(eif(cdr,e)),":" => Some(ede(cdr,e)),"m" => {Some(em(cdr,e))},"f" => Some(efn(cdr)),
  "."=>Some(ee(cdr,e)),"@"=>Some(eg(cdr,e)), "["=>Some(ecar(cdr,e)),"]"=>Some(ecdr(cdr,e)),
  _ => None}, _ => None}}
fn ea(i:&[A],e:&mut E) -> R<V<A>> { i.iter().map(|x| eval(x,e)).collect() }
fn ee(i:&[A],e:&mut E) -> R<A> {let mut r = vec![]; for x in i.iter() {
  match x {A::S(s) => r.push(eval(&parse(&tok(s.to_string())).unwrap().0,e)?),_ => r.push(eval(x,e)?)}} Ok(A::L(r))}
fn ecar(i:&[A],e:&mut E) -> R<A> {
  match eval(&i[0],e) {
    Ok(A::L(v)) => Ok(v[0].clone()),
    Ok(x) => Ok(x),
    Err(e) => Err(e)}}
fn ecdr(i:&[A],e:&mut E) -> R<A> {
  match eval(&i[0],e) {
    Ok(A::L(v)) => Ok(A::L(v[1..].to_vec())),
    Ok(x) => Ok(x),
    Err(e) => Err(e)}}
fn eg(i:&[A],e:&mut E) -> R<A> {let mut r = vec![];for x in i.iter() {
  r.push(e.get(&x.to_string()).ok_or(er!("undefined symbol"))?.clone());}Ok(A::L(r))}
fn fne<'e>(a:Rc<A>,i:&[A],o:&'e mut E) -> R<E<'e>> {
  let a = parsels(a)?;if a.len() != i.len() {return err!(&format!("expected {} args got {}", a.len(), i.len()))}
  let r = ea(i,o)?;let mut m:M<S,A> = M::new();for (k,v) in a.iter().zip(r.iter()) {m.insert(k.clone(),v.clone());}
  Ok(E{env:m,out:Some(o)})}
fn efn(i:&[A])->R<A> {
  let (p,f) = (i.first().ok_or(er!("expected cond form"))?,i.get(1).ok_or(er!("expected second form"))?);
  if i.len() > 2 {return err!("lambda can only have 2 forms")} Ok(A::G(G::n(Rc::new(p.clone()),Rc::new(f.clone()))))}
fn em(i:&[A],e:&mut E) -> R<A> {Ok(A::L(i.iter().map(|x| eval(x,e).unwrap_or(A::B(false))).collect()))}
fn eif(i:&[A],e:&mut E) -> R<A> {
  let t = i.first().ok_or(er!("expected cond form"))?; let tx = eval(t,e)?;match tx {A::B(x) => {
    let ti = if x {1} else {2};let r = i.get(ti).ok_or(er!(&format!("expected form idx={}",ti)))?;
    eval(r,e)},_ => err!(&format!("unexted cond form: {}", t.to_string()))}}
fn ede(i:&[A],e:&mut E) -> R<A> {
  let d0 = i.first().ok_or(er!("expected first form"))?;
  let ds = match d0 {A::A(x) => Ok(x.clone()), _ => err!("expected first form to be symbol")}?;
  let d1 = i.get(1).ok_or(er!("expected second form"))?;if i.len()>2 {return err!("def can only have two forms")}
  let dx = eval(d1,e)?;e.set(&ds,dx);Ok(d0.clone())}
fn ld<P:AsRef<Path>>(p:P,e:&mut E,l:bool)-> R<()> {
  let f = BufReader::new(F::open(p).or(err!("failed to open file"))?).lines();
  for i in f {if let Ok(i) = i {if let Ok((r,_)) = parse(&tok(i)) {
    match eval(&r,e) {Ok(r) => println!("{}", r),Err(e) => eprintln!("{}", e)}} else {continue}}}
  if l { repl(e) } else {Ok(())}}
fn rl()->S {let mut s = S::new(); stdin().read_line(&mut s).expect("failed to read line"); s}
fn repl(e:&mut E) -> R<()> { loop {print!("|| ");stdout().flush().or(err!("failed to display prompt"))?;
  let i = rl(); if let Ok((r,_)) = parse(&tok(i)) {
    match eval(&r, e) {Ok(r) => println!("  {}", r),Err(e) => eprintln!("{}", e)}} else {continue}}}
fn main() -> R<()> {let mut a=args().skip(1); if let Some(p) = a.next() {
  match p.as_str() { "-h" => Ok(println!(include_str!("h"))), _ => {
    let l = match a.next() {Some(i)=>i.eq("-i"),_=>false};ld(p,E::new(None).init(),l)}}} else {repl(E::new(None).init())}}
