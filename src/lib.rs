//! scm1 --- a no-fluff tree-walking toy scheme
#![feature(exact_size_is_empty)]
use std::{string::String as S, vec::Vec as V,option::Option as O, collections::HashMap as M, fs::File as F,
	  error::Error, rc::Rc,path::Path,cmp::Ordering,io::{BufReader,BufRead,stdin,stdout,Write},
	  fmt::{Display, Debug, Result as FR, Formatter as FF}};

#[derive(Debug, Clone)] pub struct Er(S); //err
impl Er {fn n(i:&str) -> Er {Er(i.to_string())}} impl Error for Er {}
impl Display for Er {fn fmt(&self, f: &mut FF<'_>) -> FR {f.write_str(&self.0)}}
macro_rules! er {($s:expr)=>{Er::n($s)}}
macro_rules! err {($s:expr)=>{Err(er!($s))}}
pub type R<X> = Result<X,Er>;

#[derive(Clone)] pub enum A {A(S),B(bool),C(S),D(f64),E,F(fn(&[A])->R<A>),G(G),L(V<A>)} //atom
impl A { fn ty(&self) -> String {use crate::A::*;(match self {
  A(_)=>"#A",B(_)=>"#B",C(_)=>"#C",D(_)=>"#D",E=>"#_",F(_)=>"#F",G(_)=>"#G",
  L(x)=>if x.is_empty() {"#_"} else {"#L"}}).to_string()}}
impl Default for A {fn default() -> A {A::E}}
impl PartialOrd for A {fn partial_cmp(&self,o:&Self) -> O<Ordering> { use crate::A::*;match self {
  A(x) => match o {A(y) => Some(x.cmp(y)),_ => None},B(x) => match o {B(y) => Some(x.cmp(y)),_ => None},
  C(x) => match o {C(y) => Some(x.cmp(y)),_ => None},D(x) => match o {D(y) => Some(x.total_cmp(y)),_ => None},
  E => None, F(_)|G(_) => None,L(x) => {let l1=x.len();match l1 {
    0 => None,1 => x[0].partial_cmp(o),
    _ => match o {L(y) => if y.len()==l1 {x.partial_cmp(&y)} else {None},_=>None}}}}}}
impl PartialEq for A {fn eq(&self, o:&A) -> bool {match &self {
  A::A(x) => match o {A::A(y) => x.eq(y),_ => false},A::B(x) => match o {A::B(y) => x.eq(y),_ => false},
  A::C(x) => match o {A::C(y) => x.eq(y),_ => false},A::D(x) => match o {A::D(y) => x.eq(y),_ => false},
  A::E => match o {A::E => true,_ => false},A::F(_)|A::G(_) => false,A::L(x) => match &o {
    A::L(y) => { for (idx,yx) in y.iter().enumerate() { 
      match x[idx].eq(yx) {true => continue, false => return false}} true},_ => false}}}}
impl Debug for A { fn fmt(&self, f: &mut FF<'_>) -> FR { use crate::A::*; match &self {
  A(_)|B(_)|C(_)|D(_)|L(_) => {f.write_str(&format!("{:?}",self))},F(_)|G(_) => {f.write_str("#fn")},E=>f.write_str("nil")}}}
impl Display for A { fn fmt(&self, f: &mut FF<'_>) -> FR {write!(f,"{}",match self {
  A::A(x) => x.clone(),A::C(x) => x.trim_matches('"').to_string().clone(),A::F(_)|A::G(_) => "#fn".to_string(),
  A::B(x) => {let r = if *x {"#t"} else {"#f"}; r.to_string()},A::D(x) => x.to_string(),A::E => "#_".to_string(),
  A::L(x) => {let xs: V<S> = x.iter().map(|x| x.to_string()).collect();format!("{}", xs.join(" "))}})}}
macro_rules! af {($a:ident,$f:expr)=>{A::F(|$a:&[A]|->R<A>{$f})}} //math
macro_rules! ab {($f:expr) => {{|i: &[A]| -> R<A> { //comp
  let (car,cdr) = (i.first().ok_or(er!("expected at least one arg"))?, &i[1..]);
  fn f(far:&A,fdr:&[A])-> bool {if let Some(x) = fdr.first() {$f(far,x) && f(x,&fdr[1..])} else {true}}
  Ok(A::B(f(car,cdr)))}}}}
#[derive(Default, Clone,PartialEq,PartialOrd)] pub struct G{args:Rc<A>,body:Rc<A>} //λ
impl G { pub fn n(args:Rc<A>,body:Rc<A>)->G {G{args,body}}}

#[derive(Default,Clone)] pub struct E<'e> {env:M<S,A>,out:O<&'e E<'e>>} impl<'e> E<'e> { //env
  pub fn n(out:O<&'e E<'e>>) -> E<'e> {E{env:M::new(),out}}
  pub fn get(&self,k:&str) -> O<&A> {
    match self.env.get(k) {Some(x) => Some(x),None => match &self.out { Some(o) => o.get(k),None=>None}}}
  pub fn set(&mut self,k:&str,v:A) -> O<A> {self.env.insert(k.to_string(),v)}
  pub fn init(&'e mut self) -> &'e mut E<'e> {
    [('$',af!(x,Ok(A::C(format!("\"{}\"", (x.into_iter().map(|x| x.to_string()).collect::<V<S>>().join(" "))))))),
     ('t',af!(x,Ok(A::C(x.iter().map(|x| x.ty()).collect::<V<S>>().join(" "))))),
     ('!',A::F(ab!(|a,b|a!=b))),('=',A::F(ab!(|a,b|a==b))),
     ('>',A::F(ab!(|a,b|a>b))),('<',A::F(ab!(|a,b|a<b))),
     (',',af!(x,Ok(A::L(x.to_vec())))),
     ('+',af!(x,Ok(A::D(parseln(x)?.into_iter().reduce(|r,a|r+a).unwrap())))),
     ('-',af!(x,Ok(A::D(parseln(x)?.into_iter().reduce(|r,a|r-a).unwrap())))),
     ('%',af!(x,Ok(A::D(parseln(x)?.into_iter().reduce(|r,a|r%a).unwrap())))),
     ('/',af!(x,Ok(A::D(parseln(x)?.into_iter().reduce(|r,a|r/a).unwrap())))),
     ('^',af!(x,Ok(A::D(parseln(x)?.into_iter().reduce(|r,a|r.powf(a)).unwrap())))),
     ('*',af!(x,{let r = parseln(x)?.iter().product(); Ok(A::D(r))})),
     ('A',A::A("".to_string())),('B',A::B(true)),('C',A::C("".to_string())),
     ('D',A::D(0.)),('E',A::E),('F',af!(_x,Ok(A::E))),('G',A::G(G::default())),
     ('L',A::L(vec![]))].map(|(x,y)| {self.env.insert(x.to_string(),y)});self}}

pub fn tok(i:S) -> V<S> { //lex
  let i = if let Some(n) = i.find(';') {&i[..n]} else {&i};if i.len() == 0 { return vec![] } else {
    let mut c = false; //instr
    let i:S = i.chars().filter_map(
      |s| match s {'"' => {c=!c; Some(s)},' ' => if c {Some(0x1a.into())} else {Some(s)}, _=>Some(s)}).collect();
    i.replace("(", " ( ").replace(")", " ) ")
      .split_ascii_whitespace().map(|x| x.to_string().replace(0x1a as char, " ")).collect()}}


pub fn parse<'a>(i:&'a[S]) -> R<(A,&'a[S])> {
  let (car, cdr) = i.split_first().ok_or(er!("could not get token"))?;
  match car.as_str() {"(" => seq(cdr), ")" => err!("unexpected `)`"),_ => Ok((atom(car), cdr))}}
fn parseln(i:&[A]) -> R<V<f64>> {i.iter().map(|x| parsen(x)).collect()}
fn parsels(i:Rc<A>) -> R<V<S>> {
  let ls = match i.as_ref() {A::L(x) => Ok(x.clone()),_ => err!("expected args form to be a list")}?;
  ls.iter().map(|x| match x {A::A(l) => Ok(l.clone()),_ => err!("expected symbols in arg list")}).collect()}
fn parsen(n:&A) -> R<f64> {match n {A::D(n) => Ok(*n), _ => err!("expected number")}}
fn seq<'a>(i:&'a [S]) -> R<(A,&'a [S])> {let mut res = vec![]; let mut xs = i;loop { //parse list
  let (car, cdr) = xs.split_first().ok_or(er!("missing closing `)`"))?;
  if car == ")" {let r = if res.is_empty() {(A::E,cdr)} else {(A::L(res), cdr)}; return Ok(r)} else {
    let (nx,ni) = parse(&xs)?;res.push(nx); xs = ni;}}}
fn atom(i:&str) -> A { match i.parse().ok() {Some(n) => A::D(n), None => { // parse atom
  if i.starts_with('"') {A::C(i.to_string().clone())} else {
    match i {"#t" => {A::B(true)},"#f" => {A::B(false)},"()"|"nil"|"#_" => A::E,i => A::A(i.to_string())}}}}}

pub fn eval(p:&A,e:&mut E) -> R<A> {match p {
  A::E => Ok(A::E),
  A::A(x) => e.get(&x).ok_or(er!(&format!("unexpected symbol `{}`", x))).map(|x| x.clone()),
  A::B(_)|A::C(_)|A::D(_) => Ok(p.clone()),A::L(x) => {
    let ll = x.len();
    let car = if ll == 0 {A::E} else {x.get(0).map(|x| x.clone()).unwrap_or(A::E)};
    let cdr = if ll < 2 {vec![]} else if ll == 2 { vec![x[1].clone()] } else { x[1..ll].to_vec() };
    match evala(&car,&cdr,e) {
      Some(r) => r,None => {let fx = eval(&car,e)?;match fx {
	A::F(f) => {let rx = cdr.iter().map(|x| eval(x,e)).collect::<R<V<A>>>();f(&rx?)},
	A::G(g) => {let fe = &mut fne(g.args,&cdr,e)?;eval(&g.body,fe)}, _ => {
	  if cdr.is_empty() {Ok(fx)}
	  else {let mut v = vec![fx]; v.extend_from_slice(&cdr); Ok(A::L(v))}}}}}},_ => err!("unexpected form")}}
fn evala(car:&A,cdr:&[A],e:&mut E) -> O<R<A>> {match car {A::A(x) => match x.as_ref() { //eval proc
  "?" => Some(eif(cdr,e)),":" => Some(ede(cdr,e)),"m" => {Some(em(cdr,e))},"f" => Some(efn(cdr)),
  "l"=>Some(el(cdr,e)),"."=>Some(ee(cdr,e)),"@"=>Some(eg(cdr,e)), "["=>Some(ecar(cdr,e)),"]"=>Some(ecdr(cdr,e)),
  "&"=>Some(eand(cdr,e)),"|"=>Some(eor(cdr,e)),_ => None}, _ => None}}
fn ea(i:&[A],e:&mut E) -> R<V<A>> { i.iter().map(|x| eval(x,e)).collect() }
fn ee(i:&[A],e:&mut E) -> R<A> {let mut r = vec![]; for x in i.iter() {
  match x {A::C(s) => r.push(eval(&parse(&tok(s.to_string())).unwrap().0,e)?),_ => r.push(eval(x,e)?)}} Ok(A::L(r))}
fn ecar(i:&[A],e:&mut E) -> R<A> { //[
  match eval(&i[0],e) {Ok(A::L(v)) => Ok(v[0].clone()),Ok(x) => Ok(x),Err(e) => Err(e)}}
fn ecdr(i:&[A],e:&mut E) -> R<A> { //]
  match eval(&i[0],e) {Ok(A::L(v)) => Ok(A::L(v[1..].to_vec())),Ok(x) => Ok(x),Err(e) => Err(e)}}
fn eand(i:&[A],e:&mut E) -> R<A> { //&
  let r = i.into_iter().map(|x| {
    match eval(x,e).unwrap_or(A::B(false)) {A::B(v) => A::B(v),A::E=>A::B(true),_=>A::B(false)}}).reduce(
    |a,b| if a == A::B(false) {A::B(false)} else if b == A::B(false) {A::B(false)} else {A::B(true)});
  r.ok_or(er!("& failed"))}
fn eor(i:&[A],e:&mut E) -> R<A> { //|
  let r = i.into_iter().map(|x| {
    match eval(x,e).unwrap_or(A::B(false)) {A::B(v) => A::B(v),A::E=>A::B(true),_=>A::B(false)}}).reduce(
    |a,b| if a == A::B(true) {A::B(true)} else if b == A::B(true) {A::B(true)} else {A::B(false)});
  r.ok_or(er!("| failed"))}
fn el(i:&[A],e:&mut E) -> R<A> { //l
  let r:V<A> = i.iter().map(|x| eval(x,e).unwrap_or(A::E)).collect();
  for x in  r {match x {A::C(_)=>ld(x.to_string(),e,false)?,_=>()}};Ok(A::E)}
fn eg(i:&[A],e:&mut E) -> R<A> {let mut r = vec![];for x in i.iter() { //@
  r.push(e.get(&x.to_string()).ok_or(er!("undefined symbol"))?.clone());}Ok(A::L(r))}
fn fne<'e>(a:Rc<A>,i:&[A],o:&'e mut E) -> R<E<'e>> {
  let mut a = parsels(a)?;let l1=a.len();let l2=i.len();
  if l1 > l2 {return err!(&format!("expected {} args got {}", l1, l2))}
  if l1 < l2 {for _ in 0..l2-l1 {a.push(A::E.to_string())}};let r = ea(i,o)?;let mut m:M<S,A> = M::new();
  for (k,v) in a.iter().zip(r.iter()) {m.insert(k.clone(),v.clone());} Ok(E{env:m,out:Some(o)})}
fn efn(i:&[A])->R<A> { //f
  let (p,f) = (i.first().ok_or(er!("expected cond form"))?,i.get(1).ok_or(er!("expected second form"))?);
  if i.len() > 2 {return err!("lambda can only have 2 forms")} Ok(A::G(G::n(Rc::new(p.clone()),Rc::new(f.clone()))))}
fn em(i:&[A],e:&mut E) -> R<A> {Ok(A::L(i.iter().map(|x| eval(x,e).unwrap_or(A::E)).collect()))} //m
fn eif(i:&[A],e:&mut E) -> R<A> { //?
  let t = i.first().ok_or(er!("expected cond form"))?; let tx = eval(t,e)?;match tx {A::B(x) => {
    let ti = if x {1} else {2};let r = i.get(ti).ok_or(er!(&format!("expected form idx={}",ti)))?;
    eval(r,e)},_ => err!(&format!("unexted cond form: {}", t.to_string()))}}
fn ede(i:&[A],e:&mut E) -> R<A> { //:
  let d0 = i.first().ok_or(er!("expected first form"))?;
  let ds = match d0 {A::A(x) => Ok(x.clone()), _ => err!("expected first form to be symbol")}?;
  let d1 = i.get(1).ok_or(er!("expected second form"))?;if i.len()>2 {return err!("def can only have two forms")}
  let dx = eval(d1,e)?;e.set(&ds,dx);Ok(d0.clone())}

pub fn ld<P:AsRef<Path>>(p:P,e:&mut E,i:bool)-> R<()> { //load file
  let p = p.as_ref(); if p.is_dir() {for x in p {ld(x,e,i)?}}
  let f = BufReader::new(F::open(p).or(err!("failed to open file"))?).lines();
  for x in f {if let Ok(r) = x {if let Ok((r,_)) = parse(&tok(r)) {
    match eval(&r,e) {Ok(r) => println!("{}", r),Err(e) => eprintln!("{}", e)}} else {continue}}}
  if i { repl(e) } else {Ok(())}}
#[inline(always)] pub fn run(i:&str,e:&mut E) -> S {
  match parse(&tok(i.to_string())) {
    Ok((x,_)) => match eval(&x,e) {Ok(x) => x.to_string(),Err(e) => e.to_string()},
    Err(e) => e.to_string()}}
#[inline(always)] fn rl()->R<S> {
  let mut s = S::new(); stdin().read_line(&mut s).map_err(|e| er!(&e.to_string()))?;Ok(s)}
pub fn repl(e:&mut E) -> R<()> {
  println!("{} {}", env!("CARGO_PKG_NAME"),env!("CARGO_PKG_VERSION"));
  loop { print!("  λ ");stdout().flush().or(err!("failed to display prompt"))?;
	 let i = rl()?; if let Ok((r,_)) = parse(&tok(i)) {
	   match eval(&r, e) {Ok(r) => println!("  {}", r),Err(e) => eprintln!("{}", e)}} else {continue}}}
