//! scm1 --- a no-fluff tree-walking toy scheme
#![feature(never_type)]
use {String as S, Vec as V, Box as B,
     std::{option::Option as O, collections::HashMap as M, io::stdin, error::Error,
	   fmt::{Display, Debug, Result as FR, Formatter as FF}}};
#[derive(Debug)] struct Er(S);
impl Error for Er {}
impl Er {fn n(i:&str) -> Er {Er(i.to_string())}}
impl Display for Er {fn fmt(&self, f: &mut FF<'_>) -> FR {f.write_str(&self.0)}}
type R<X> = Result<X,Er>;
#[derive(Clone)] enum A {A(S),B(bool),N(f64),S(S),F(fn(&[A])->R<A>),L(V<A>)}
impl PartialEq for A {
  fn eq(&self, rhs:&A) -> bool {
    match &self {
      A::F(x) => false,
      A::L(x) => match &rhs {
	A::L(x) => { for (idx,y) in x.iter().enumerate() { 
	  match x[idx].eq(y) {true => continue, false => return false}
	} true},
	_ => false}, _ => false,}}} // TODO
impl Debug for A { fn fmt(&self, f: &mut FF<'_>) -> FR { use crate::A::*; match &self {
  A(_)|B(_)|N(_)|S(_)|L(_) => {f.write_str(&format!("{:?}",self))},
  F(_) => {f.write_str("#'fn")}}}}
impl Display for A { fn fmt(&self, f: &mut FF<'_>) -> FR {
  let r = match self {
    A::A(x) => x.clone(),
    A::B(x) => x.to_string(),A::N(x) => x.to_string(),
    A::S(x) => format!("\"{}\"",x),
    A::F(_) => "#'fn".to_string(),
    A::L(x) => {let xs: V<S> = x.iter().map(|x| x.to_string()).collect();
		format!("({})", xs.join(" "))}};
  write!(f,"{}",r)}}
macro_rules! af {($a:ident,$f:expr)=>{A::F(|$a:&[A]|->R<A>{$f})}}
macro_rules! ab {($f:expr) => {{
  |i: &[A]| -> R<A> { let ln = parseln(i)?;
		      let (car,cdr) = (ln.first().ok_or(Er::n("expected at least one number"))?, &ln[1..]);
		      fn f(far:&f64,fdr:&[f64])-> bool {
			match fdr.first() {Some(x) => $f(far,x) && f(x,&fdr[1..]),None => true}}
		      Ok(A::B(f(car,cdr)))}}}}
type P = V<A>;
struct FnT (M<S,A>);
struct SmT (M<S,A>);
struct E {env:M<S,A>,out: O<B<E>>}
impl E {
  fn new(out:O<B<E>>) -> E {E{env:M::new(),out}}
  fn init(&mut self) -> &mut E {
    [('m', af!(x,Ok(A::L(x.to_vec())))),
     ('+',af!(x,{let r = parseln(x)?.iter().fold(0.0, |r,a| r+a); Ok(A::N(r))})),
     ('-',af!(x,{let r = parseln(x)?.iter().fold(0.0, |r,a| r-a); Ok(A::N(r))})),
     ('*',af!(x,{let r = parseln(x)?.iter().fold(1.0, |r,a| r*a); Ok(A::N(r))})),
     ('/',af!(x,{let r = parseln(x)?.iter().fold(1.0, |r,a| r/a); Ok(A::N(r))})),
     ('!',A::F(ab!(|a,b|a!=b))),
     ('=',A::F(ab!(|a,b|a==b))),
     ('>',A::F(ab!(|a,b|a>b))),
     ('<',A::F(ab!(|a,b|a<b)))
    ].map(|(x,y)| {self.env.insert(x.to_string(),y)});self}
  fn get(&self,k:&str) -> O<&A> {self.env.get(k)}
  fn set(&mut self,k:&str,v:A) -> O<A> {self.env.insert(k.to_string(),v)}}
fn tok(i:S) -> V<S> {i.replace("(", " ( ").replace(")", " ) ").split_whitespace().map(|x| x.to_string()).collect()}
fn parseln(ln:&[A]) -> R<V<f64>> {ln.iter().map(|x| parsen(x)).collect()}
fn parsen(n:&A) -> R<f64> {match n {A::N(n) => Ok(*n), _ => Err(Er::n("expected number"))}}
fn parses(s:&A) -> R<S> {match s {A::S(s) => Ok(s.clone()), _ => Err(Er::n("expected string"))}}
fn parse<'a>(i:&'a[S]) -> R<(A,&'a[S])> {
  let (car, cdr) = i.split_first().ok_or(Er::n("could not get token"))?;
  match &car[..] {"(" => seq(cdr),")" => Err(Er::n("unexpected `)`")),_ => Ok((atom(car), cdr))}}
fn seq<'a>(i:&'a [S]) -> R<(A,&'a [S])> {
  let mut res:P = vec![]; let mut xs = i;
  loop {let (car, cdr) = xs.split_first().ok_or(Er::n("missing closing `)`"))?;
	if car == ")" {return Ok((A::L(res), cdr))}
	let (nx,ni) = parse(&xs)?;res.push(nx); xs = ni;}}
fn atom(i:&str) -> A {match i.parse().ok() {
  Some(n) => A::N(n),None => {if i.starts_with('\"') {A::S(i.to_string())} else {A::A(i.to_string().clone())}}}}
fn eval(p:&A,e:&mut E) -> R<A> {match p {
  A::A(x) => e.get(&x).ok_or(Er::n(&format!("unexpected symbol `{}`", x))).map(|x| x.clone()),
  A::B(_)|A::N(_) => Ok(p.clone()),
  A::L(x) => { let far = x.first().ok_or(Er::n("expected non-empty list"))?;
	       let fdr = &x[1..];
	       match evala(far,fdr,e) {
		 Some(r) => r,
		 None => {let fx = eval(far,e)?;
			  match fx {
			    A::F(f) => {
			      let rx = fdr.iter().map(|x| eval(x,e)).collect::<R<V<A>>>();
			      f(&rx?)},
			    _ => Err(Er::n("first form must be a function"))}}}},
  _ => Err(Er::n("unexpected form"))}}
fn evala(car:&A,cdr:&[A],e:&mut E) -> O<R<A>> {
  match car {
    A::A(x) => match x.as_ref() {
      "?" => Some(eif(cdr,e)),
      ":" => Some(ede(cdr,e)),
      _ => None}, _ => None}}
fn eif(i:&[A],e:&mut E) -> R<A> {
  let t = i.first().ok_or(Er::n("expected cond form"))?; let tx = eval(t,e)?;
  match tx {
    A::B(x) => {
      let ti = if x {1} else {2};let r = i.get(ti).ok_or(Er(format!("expected form idx={}",ti)))?;
      eval(r,e)},
    _ => Err(Er(format!("unexted cond form: {}", t.to_string())))}}
fn ede(i:&[A],e:&mut E) -> R<A> {
  let d0 = i.first().ok_or(Er::n("expected first form"))?;
  let ds = match d0 {A::A(x) => Ok(x.clone()), _ => Err(Er::n("expected first form to be symbol"))}?;
  let d1 = i.get(1).ok_or(Er::n("expected second form"))?;
  if i.len()>2 {return Err(Er::n("def can only have two forms"))}
  let dx = eval(d1,e)?;e.set(&ds,dx);Ok(d0.clone())}
fn rl()->S { let mut s = S::new(); stdin().read_line(&mut s).expect("failed to read line"); s}
fn repl(e:&mut E) -> R<!> { loop {
  let i = rl(); let (r,_) = parse(&tok(i))?;
  match eval(&r, e) {Ok(r) => println!("=> {}", r),Err(e) => match e { Er(e) => eprintln!("{}", e) }}}}
fn main() -> R<!> {println!("SCM1 --- a no-fluff tree-walking toy scheme");repl(E::new(None).init())}
