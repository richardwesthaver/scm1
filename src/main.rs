//! scm1 --- a no-fluff tree-walking toy scheme
use {String as S, Vec as V,
     std::{option::Option as O, collections::HashMap as M, error::Error, rc::Rc,
	   io::{stdin,stdout,Write},fmt::{Display, Debug, Result as FR, Formatter as FF}}};
#[derive(Debug)] struct Er(S);
impl Error for Er {}
impl Er {fn n(i:&str) -> Er {Er(i.to_string())}}
impl Display for Er {fn fmt(&self, f: &mut FF<'_>) -> FR {f.write_str(&self.0)}}
type R<X> = Result<X,Er>;
macro_rules! er {($s:expr)=>{Er::n($s)}} macro_rules! err {($s:expr)=>{Err(er!($s))}}
#[derive(Clone)] enum A {A(S),B(bool),N(f64),S(S),F(fn(&[A])->R<A>),G(G),L(V<A>)}
#[derive(Clone)] struct G{args:Rc<A>,body:Rc<A>}
impl G { fn n(args:Rc<A>,body:Rc<A>)->G {G{args,body}}}
impl PartialEq for A {
  fn eq(&self, rhs:&A) -> bool {
    match &self {
      A::F(_) => false,
      A::G(_) => false,
      A::L(x) => match &rhs {
	A::L(y) => { for (idx,yx) in y.iter().enumerate() { 
	  match x[idx].eq(yx) {true => continue, false => return false}} true},_ => false} _ => false,}}} // TODO
impl Debug for A { fn fmt(&self, f: &mut FF<'_>) -> FR { use crate::A::*; match &self {
  A(_)|B(_)|N(_)|S(_)|L(_) => {f.write_str(&format!("{:?}",self))},F(_)|G(_) => {f.write_str("#'function")}}}}
impl Display for A { fn fmt(&self, f: &mut FF<'_>) -> FR {
  let r = match self {
    A::A(x) => x.clone(),A::B(x) => x.to_string(),A::N(x) => x.to_string(),
    A::S(x) => format!("\"{}\"",x),A::F(_)|A::G(_) => "#'fn".to_string(),
    A::L(x) => {let xs: V<S> = x.iter().map(|x| x.to_string()).collect();format!("({})", xs.join(" "))}};
  write!(f,"{}",r)}}
macro_rules! af {($a:ident,$f:expr)=>{A::F(|$a:&[A]|->R<A>{$f})}}
macro_rules! ab {($f:expr) => {{
  |i: &[A]| -> R<A> { let ln = parseln(i)?;
		      let (car,cdr) = (ln.first().ok_or(er!("expected at least one number"))?, &ln[1..]);
		      fn f(far:&f64,fdr:&[f64])-> bool {
			match fdr.first() {Some(x) => $f(far,x) && f(x,&fdr[1..]),None => true}}
		      Ok(A::B(f(car,cdr)))}}}}
//struct FnT (M<S,A>);struct SmT (M<S,A>);
#[derive(Default,Clone)] struct E<'e> {env:M<S,A>,out:O<&'e E<'e>>} // TODO out
impl<'e> E<'e> {
  fn new(out:O<&'e E<'e>>) -> E<'e> {E{env:M::new(),out}}
  fn get(&self,k:&str) -> O<&A> {
    match self.env.get(k) {
      Some(x) => Some(x),
      None => match &self.out { Some(o) => o.get(k),None=>None}}}
  fn set(&mut self,k:&str,v:A) -> O<A> {self.env.insert(k.to_string(),v)}
  fn init(&'e mut self) -> &'e mut E<'e> {
    [//('m', af!(x,Ok(A::L(x.to_vec())))), // add to builtins
      ('+',af!(x,Ok(A::N(parseln(x)?.iter().fold(0.0, |r,a| r+a))))),
      ('-',af!(x,Ok(A::N(parseln(x)?.iter().fold(0.0, |r,a| r-a))))),
      ('%',af!(x,Ok(A::N(parseln(x)?.into_iter().reduce(|r,a| r%a).unwrap())))),
      ('/',af!(x,Ok(A::N(parseln(x)?.into_iter().reduce(|r,a| r/a).unwrap())))),
      ('^',af!(x,Ok(A::N(parseln(x)?.into_iter().reduce(|r,a| r.powf(a)).unwrap())))),
      ('*',af!(x,{let r = parseln(x)?.iter().product(); Ok(A::N(r))})),
      ('!',A::F(ab!(|a,b|a!=b))),('=',A::F(ab!(|a,b|a==b))),('>',A::F(ab!(|a,b|a>b))),
      ('<',A::F(ab!(|a,b|a<b)))].map(|(x,y)| {self.env.insert(x.to_string(),y)});self}}
fn tok(i:S) -> V<S> {i.replace("(", " ( ").replace(")", " ) ").split_whitespace().map(|x| x.to_string()).collect()}
fn parseln(i:&[A]) -> R<V<f64>> {i.iter().map(|x| parsen(x)).collect()}
fn parsels(i:Rc<A>) -> R<V<S>> {
  let ls = match i.as_ref() {A::L(x) => Ok(x.clone()),_ => err!("expected args form to be a list")}?;
  ls.iter().map(|x| match x {
    A::A(l) => Ok(l.clone()),
    _ => err!("expected symbols in arg list")}).collect()}
fn parsen(n:&A) -> R<f64> {match n {A::N(n) => Ok(*n), _ => err!("expected number")}}
//fn parses(s:&A) -> R<S> {match s {A::S(s) => Ok(s.clone()), _ => err!("expected string")}}
fn parse<'a>(i:&'a[S]) -> R<(A,&'a[S])> {
  let (car, cdr) = i.split_first().ok_or(Er::n("could not get token"))?;
  match &car[..] {"(" => seq(cdr),")" => err!("unexpected `)`"),_ => Ok((atom(car), cdr))}}
fn seq<'a>(i:&'a [S]) -> R<(A,&'a [S])> {
  let mut res = vec![]; let mut xs = i;
  loop {let (car, cdr) = xs.split_first().ok_or(Er::n("missing closing `)`"))?;
	if car == ")" {return Ok((A::L(res), cdr))}
	let (nx,ni) = parse(&xs)?;res.push(nx); xs = ni;}}
fn atom(i:&str) -> A {match i.parse().ok() {
  Some(n) => A::N(n),
  None => {if i.starts_with('\"') {A::S(i.to_string())} else {A::A(i.to_string().clone())}}}}
fn eval(p:&A,e:&mut E) -> R<A> {match p {
  A::A(x) => e.get(&x).ok_or(Er::n(&format!("unexpected symbol `{}`", x))).map(|x| x.clone()),
  A::B(_)|A::N(_) => Ok(p.clone()),
  A::L(x) => { let (car,cdr) = (x.first().ok_or(Er::n("expected non-empty list"))?, &x[1..]);
	       match evala(car,cdr,e) {
		 Some(r) => r,
		 None => {let fx = eval(car,e)?;
			  match fx {
			    A::F(f) => {
			      let rx = cdr.iter().map(|x| eval(x,e)).collect::<R<V<A>>>();
			      f(&rx?)},
			    A::G(g) => {
			      let fe = &mut fne(g.args,cdr,e)?;
			      eval(&g.body,fe)},
			    _ => err!("first form must be a function")}}}},
  _ => err!("unexpected form")}}
fn evala(car:&A,cdr:&[A],e:&mut E) -> O<R<A>> {match car {
  A::A(x) => match x.as_ref() {
    "?" => Some(eif(cdr,e)),":" => Some(ede(cdr,e)), "f" => Some(efn(cdr)),
    _ => None}, _ => None}}
fn ea(i:&[A],e:&mut E) -> R<V<A>> { i.iter().map(|x| eval(x,e)).collect() }
fn fne<'e>(a:Rc<A>,i:&[A],o:&'e mut E) -> R<E<'e>> {
  let a = parsels(a)?;
  if a.len() != i.len() { return err!(&format!("expected {} args got {}", a.len(), i.len())) }
  let r = ea(i,o)?;
  let mut m:M<S,A> = M::new();
  for (k,v) in a.iter().zip(r.iter()) { m.insert(k.clone(),v.clone()); }
  Ok(E{env:m,out:Some(o)})}
fn efn(i:&[A])->R<A> {
  let p = i.first().ok_or(er!("expected cond form"))?;
  let f = i.get(1).ok_or(er!("expected second form"))?;
  if i.len() > 2 {return err!("lambda can only have 2 forms")}
  Ok(A::G(G::n(Rc::new(p.clone()),Rc::new(f.clone()))))}
fn eif(i:&[A],e:&mut E) -> R<A> {
  let t = i.first().ok_or(Er::n("expected cond form"))?; let tx = eval(t,e)?;
  match tx {
    A::B(x) => {
      let ti = if x {1} else {2};let r = i.get(ti).ok_or(Er(format!("expected form idx={}",ti)))?;
      eval(r,e)},_ => err!(&format!("unexted cond form: {}", t.to_string()))}}
fn ede(i:&[A],e:&mut E) -> R<A> {
  let d0 = i.first().ok_or(Er::n("expected first form"))?;
  let ds = match d0 {A::A(x) => Ok(x.clone()), _ => err!("expected first form to be symbol")}?;
  let d1 = i.get(1).ok_or(Er::n("expected second form"))?;
  if i.len()>2 {return err!("def can only have two forms")}
  let dx = eval(d1,e)?;e.set(&ds,dx);Ok(d0.clone())}
fn rl()->S {let mut s = S::new(); stdin().read_line(&mut s).expect("failed to read line"); s}
fn repl(e:&mut E) -> R<()> { loop {print!("|| ");stdout().flush().or(err!("failed to display prompt"))?;
  let i = rl(); if let Ok((r,_)) = parse(&tok(i)) {
    match eval(&r, e) {Ok(r) => println!("  {}", r),Err(e) => eprintln!("{}", e)}} else {continue}}}
fn main() -> R<()> {println!("SCM1 --- a no-fluff tree-walking toy scheme");repl(E::new(None).init())}
