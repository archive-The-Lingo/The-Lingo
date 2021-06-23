#![no_std]
#[cfg(feature = "no_std")]
extern crate no_std_compat as std;
#[cfg(feature = "std")]
extern crate std;

pub mod trace;

use std::prelude::v1::*;
use std::ptr;
use std::fmt::Debug;
use std::ops::Deref;
use std::path::Path;
use std::sync::{Arc, Mutex, Weak};
use std::vec;

use arc_swap::ArcSwap;
use downcast_rs::Downcast;
use downcast_rs::impl_downcast;
use lazy_static::lazy_static;
use trilean::SKleene;
use weak_table::PtrWeakHashSet;
use num_bigint::BigUint;
use bitvec::prelude::*;

// todo: add Trace and Hash
pub trait Values: Downcast + Debug + Send + Sync {
    fn deoptimize(&self) -> CoreValue;
    fn internal_equal(&self, _this: &Value, _other: &Value) -> SKleene {
        SKleene::Unknown
    }
}
impl_downcast!(Values);

type ValueInternal = Arc<dyn Values>;

#[derive(Debug, Clone)]
pub struct Value(ValueInternal);

type WeakValue = Weak<dyn Values>;

impl Deref for Value {
    type Target = Arc<dyn Values>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl Value {
    pub fn new<T: Values>(x: T) -> Value {
        Value(Arc::new(x))
    }
    pub fn equal(&self, other: &Value) -> bool {
        if ptr::eq::<dyn Values>(&***self, &***other) { return true; }
        match self.internal_equal(other) {
            SKleene::False => false,
            SKleene::True => true,
            SKleene::Unknown => self.deoptimize().core_equal(&other.deoptimize()),
        }
    }
    pub fn internal_equal(&self, other: &Value) -> SKleene {
        self.0.internal_equal(self, other)
    }
    pub fn from_bool(x: bool) -> Value {
        if x {
            TRUE.clone()
        } else {
            FALSE.clone()
        }
    }
}

#[derive(Debug)]
pub struct OptimizableValue(ArcSwap<Value>);

impl Deref for OptimizableValue {
    type Target = ArcSwap<Value>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl OptimizableValue {
    fn remove_layers(&self) -> Arc<Value> {
        let mut this = self.load().clone();
        while let Some(this0) = this.downcast_ref::<OptimizableValue>() {
            this = this0.load().clone();
        }
        this
    }
}

impl Values for OptimizableValue {
    fn deoptimize(&self) -> CoreValue {
        let inner: Value = (&**self.load()).clone();
        inner.deoptimize()
    }

    fn internal_equal(&self, _this: &Value, other: &Value) -> SKleene {
        if let Some(other) = other.downcast_ref::<OptimizableValue>() {
            if ptr::eq::<ArcSwap<Value>>(&**self, &**other) { return SKleene::True; }
            let this = self.remove_layers();
            if ptr::eq::<Value>(&*this, &*other.remove_layers()) {
                self.store(this.clone());
                other.store(this.clone());
                return SKleene::True;
            }
        }
        match self.load().internal_equal(other) {
            SKleene::True => {
                self.store(Arc::new(other.clone()));
                SKleene::True
            }
            x => x,
        }
    }
}

pub type CoreIdentifier = String;

#[derive(Debug, Clone)]
pub enum CoreValue {
    EmptyList,
    Symbol(CoreIdentifier),
    NonEmptyList(Value, Value),
    Tagged(Value, Value),
    Exception(Value, Value),
}

impl Values for CoreValue {
    fn deoptimize(&self) -> CoreValue {
        self.clone()
    }
    fn internal_equal(&self, this: &Value, other: &Value) -> SKleene {
        if let Some(other) = other.downcast_ref::<CoreValue>() {
            SKleene::from_bool(self.core_equal(other))
        } else {
            other.internal_equal(this)
        }
    }
}

impl CoreValue {
    pub fn core_equal(&self, other: &CoreValue) -> bool {
        match (self, other) {
            (CoreValue::EmptyList, CoreValue::EmptyList) => true,
            (CoreValue::Symbol(x), CoreValue::Symbol(y)) => x == y,
            (CoreValue::NonEmptyList(x0, y0), CoreValue::NonEmptyList(x1, y1)) => x0.equal(x1) && y0.equal(y1),
            (CoreValue::Tagged(x0, y0), CoreValue::Tagged(x1, y1)) => x0.equal(x1) && y0.equal(y1),
            (CoreValue::Exception(x0, y0), CoreValue::Exception(x1, y1)) => x0.equal(x1) && y0.equal(y1),
            (_, _) => false,
        }
    }
}

lazy_static! {
    pub static ref EMPTY_LIST: Value = Value::new(CoreValue::EmptyList);
}

use std::slice::Iter;

impl From<Iter<'_, Value>> for CoreValue {
    fn from(xs: Iter<'_, Value>) -> Self {
        let mut result = CoreValue::EmptyList;
        for x in xs.rev() {
            result = CoreValue::NonEmptyList(x.clone(), Value::new(result));
        }
        result
    }
}

impl From<&Vec<Value>> for CoreValue {
    fn from(xs: &Vec<Value>) -> Self {
        let mut result = CoreValue::EmptyList;
        for x in xs.iter().rev() {
            result = CoreValue::NonEmptyList(x.clone(), Value::new(result));
        }
        result
    }
}

impl From<&Vec<Value>> for Value {
    fn from(xs: &Vec<Value>) -> Self {
        let mut result = EMPTY_LIST.clone();
        for x in xs.iter().rev() {
            result = Value::new(CoreValue::NonEmptyList(x.clone(), result));
        }
        result
    }
}

impl From<Vec<Value>> for Value {
    fn from(xs: Vec<Value>) -> Self {
        let mut result = EMPTY_LIST.clone();
        for x in xs.into_iter().rev() {
            result = Value::new(CoreValue::NonEmptyList(x, result));
        }
        result
    }
}

#[macro_export]
macro_rules! list {
    () => {EMPTY_LIST.clone()};
    ( $a:expr ) => {Value::new(CoreValue::NonEmptyList($a.clone(), EMPTY_LIST.clone()))};
    ( $a:expr, $( $x:expr ),* ) => {
        Value::new(CoreValue::NonEmptyList($a.clone(), list!($($x),*)))
    };
}

pub type Identifier = Value;

#[derive(Debug, Clone)]
pub enum Expression {
    Id(Identifier),
    Quote(Value),
    ApplyFunction(Arc<Expression>, Vec<Expression>),
    ApplyMacro(Arc<Expression>, Vec<Value>),
    Comment(Arc<Expression>, Value),
    Builtin(ExpressionBuiltin),
    Positioned(Arc<Expression>, Position),
}

pub type Position = Arc<UNIXFilePosition>;

#[derive(Debug, Clone)]
pub struct UNIXFilePosition {
    file: Arc<Path>,
    line: u128,
    column: u128,
    name: Option<String>,
}

impl Values for Expression {
    fn deoptimize(&self) -> CoreValue {
        match self {
            Expression::Id(_) => todo!(),
            Expression::Quote(_) => todo!(),
            Expression::ApplyFunction(_, _) => todo!(),
            Expression::ApplyMacro(_, _) => todo!(),
            Expression::Comment(_, _) => todo!(),
            Expression::Builtin(x) => x.deoptimize(),
            Expression::Positioned(_, _) => todo!(),
        }
    }
}

pub mod name;

pub fn internal_exception(why: &Value, environment: &Mapping, what: &Expression) -> Value {
    Value::new(CoreValue::Exception(name::value::CORE.clone(), list!(why,Value::new(environment.clone()),Value::new(what.clone()))))
}

impl Expression {
    pub fn evaluate(&self, environment: &Mapping) -> Value {
        self.evaluate_with_option_stack(environment, &None)
    }
    pub fn evaluate_with_option_stack(&self, environment: &Mapping, stack: &Option<DebugStack>) -> Value {
        match self {
            Expression::Id(x) => match environment.get(x) {
                Some(v) => v,
                None => internal_exception(&name::value::EXCEPTION_NODEF, environment, self),
            },
            Expression::Quote(x) => x.clone(),
            Expression::ApplyFunction(_, _) => todo!(),
            Expression::ApplyMacro(_, _) => todo!(),
            Expression::Comment(exp, _cmt) => exp.evaluate_with_option_stack(environment, stack),
            Expression::Builtin(x) => x.evaluate_with_option_stack(environment, stack),
            Expression::Positioned(expression, position) => expression.evaluate_with_option_stack(environment, &if let Some(stack) = stack { Some(stack.extend(position)) } else { None }),
        }
    }
}

#[derive(Debug, Clone)]
pub enum ExpressionBuiltin {
    IsEmptyList(Arc<Expression>),
    IsSymbol(Arc<Expression>),
    NewSymbol(Arc<Expression>),
    ReadSymbol(Arc<Expression>),
    IsNonEmptyList(Arc<Expression>),
    ReadNonEmptyListHead(Arc<Expression>),
    ReadNonEmptyListTail(Arc<Expression>),
    NewNonEmptyList(Arc<Expression>, Arc<Expression>),
    IsTagged(Arc<Expression>),
    ReadTaggedTag(Arc<Expression>),
    ReadTaggedData(Arc<Expression>),
    NewTagged(Arc<Expression>, Arc<Expression>),
    IsException(Arc<Expression>),
    ReadExceptionTag(Arc<Expression>),
    ReadExceptionData(Arc<Expression>),
    NewException(Arc<Expression>, Arc<Expression>),
    Recursive(Identifier, Arc<Expression>),
    Evaluate(Arc<Expression>, Arc<Expression>),
    Lambda(Vec<Identifier>, Option<Identifier>, Arc<Expression>),
    ReadBoolean(Arc<Expression>, Arc<Expression>, Arc<Expression>),

    // for easy using - could be implemented in the lingo itself
    IsBoolean(Arc<Expression>),
    IsMapping(Arc<Expression>),
    ReadMapping(Arc<Expression>, Arc<Expression>),
}


lazy_static! {
    pub static ref TRUE: Value = Value::new(CoreValue::Tagged(name::value::TRUE.clone(),list!()));
    pub static ref FALSE: Value = Value::new(CoreValue::Tagged(name::value::FALSE.clone(),list!()));
}

impl From<bool> for Value {
    fn from(x: bool) -> Self {
        if x {
            TRUE.clone()
        } else {
            FALSE.clone()
        }
    }
}

impl ExpressionBuiltin {
    pub fn evaluate(&self, environment: &Mapping) -> Value {
        self.evaluate_with_option_stack(environment, &None)
    }
    pub fn evaluate_with_option_stack(&self, environment: &Mapping, stack: &Option<DebugStack>) -> Value {
        let eval = |x: &Expression| x.evaluate_with_option_stack(environment, stack);
        let exception = |why: &Value| internal_exception(why, environment, &Expression::Builtin(self.clone()));
        let typemismatch = || exception(&name::value::EXCEPTION_TYPEMISMATCH);
        match self {
            ExpressionBuiltin::IsEmptyList(x) => if let CoreValue::EmptyList = eval(x).deoptimize() { TRUE.clone() } else { FALSE.clone() },
            ExpressionBuiltin::IsSymbol(x) => if let CoreValue::Symbol(_) = eval(x).deoptimize() { TRUE.clone() } else { FALSE.clone() },
            ExpressionBuiltin::NewSymbol(_) => todo!(),
            ExpressionBuiltin::ReadSymbol(x) => if let CoreValue::Symbol(s) = eval(x).deoptimize() { Value::new(CharString(s)) } else { typemismatch() },
            ExpressionBuiltin::IsNonEmptyList(x) => if let CoreValue::NonEmptyList(_, _) = eval(x).deoptimize() { TRUE.clone() } else { FALSE.clone() },
            ExpressionBuiltin::ReadNonEmptyListHead(x) => if let CoreValue::NonEmptyList(v, _) = eval(x).deoptimize() { v } else { typemismatch() },
            ExpressionBuiltin::ReadNonEmptyListTail(x) => if let CoreValue::NonEmptyList(_, v) = eval(x).deoptimize() { v } else { typemismatch() },
            ExpressionBuiltin::NewNonEmptyList(x, y) => Value::new(CoreValue::NonEmptyList(eval(x),eval(y))),
            ExpressionBuiltin::IsTagged(x) => if let CoreValue::Tagged(_, _) = eval(x).deoptimize() { TRUE.clone() } else { FALSE.clone() },
            ExpressionBuiltin::ReadTaggedTag(x) => if let CoreValue::Tagged(v, _) = eval(x).deoptimize() { v } else { typemismatch() },
            ExpressionBuiltin::ReadTaggedData(x) => if let CoreValue::Tagged(_, v) = eval(x).deoptimize() { v } else { typemismatch() },
            ExpressionBuiltin::NewTagged(x, y) => Value::new(CoreValue::Tagged(eval(x),eval(y))),
            ExpressionBuiltin::IsException(x) => if let CoreValue::Exception(_, _) = eval(x).deoptimize() { TRUE.clone() } else { FALSE.clone() },
            ExpressionBuiltin::ReadExceptionTag(x) => if let CoreValue::Exception(v, _) = eval(x).deoptimize() { v } else { typemismatch() },
            ExpressionBuiltin::ReadExceptionData(x) => if let CoreValue::Exception(_, v) = eval(x).deoptimize() { v } else { typemismatch() },
            ExpressionBuiltin::NewException(x, y) => Value::new(CoreValue::Exception(eval(x),eval(y))),
            ExpressionBuiltin::Recursive(_, _) => todo!(),
            ExpressionBuiltin::Evaluate(_, _) => todo!(),
            ExpressionBuiltin::Lambda(_, _, _) => todo!(),
            ExpressionBuiltin::ReadBoolean(_, _, _) => todo!(),
            ExpressionBuiltin::IsBoolean(_) => todo!(),
            ExpressionBuiltin::IsMapping(_) => todo!(),
            ExpressionBuiltin::ReadMapping(_, _) => todo!(),
        }
    }
}

impl Values for ExpressionBuiltin {
    fn deoptimize(&self) -> CoreValue {
        todo!()
    }
}

// TODO: optimize this.
#[derive(Debug, Clone)]
pub struct Mapping(Arc<ArcLinkedList<(Value, Value)>>);

impl Mapping {
    pub fn get(&self, key: &Value) -> Option<Value> {
        let mut list = &self.0;
        while let ArcLinkedList::NonEmpty((k0, v0), tail) = &**list {
            if k0.equal(key) {
                return Some(v0.clone());
            }
            list = tail;
        }
        None
    }
    pub fn extend(&self, key: Value, value: Value) -> Mapping {
        Mapping(Arc::new(ArcLinkedList::NonEmpty((key, value), self.0.clone())))
    }
}

impl Values for Mapping {
    fn deoptimize(&self) -> CoreValue {
        todo!()
    }
}

#[derive(Debug, Clone)]
pub struct DebugStack(Arc<ArcLinkedList<Position>>);

impl DebugStack {
    pub fn extend(&self, x: &Position) -> Self {
        DebugStack(Arc::new(ArcLinkedList::NonEmpty(x.clone(), self.0.clone())))
    }
}

#[derive(Debug, Clone)]
pub enum ArcLinkedList<T> {
    Empty,
    NonEmpty(T, Arc<ArcLinkedList<T>>),
}

lazy_static! {
    static ref POSSIBLY_RECURSIVE_SET: Mutex<PtrWeakHashSet<WeakValue>> = Mutex::new(PtrWeakHashSet::new());
}

#[derive(Debug)]
pub struct PossiblyRecursive(ArcSwap<Value>);

impl PossiblyRecursive {
    pub fn new(x: &Value) -> Self {
        POSSIBLY_RECURSIVE_SET.lock().unwrap().insert((**x).clone());
        PossiblyRecursive(ArcSwap::from(Arc::new(x.clone())))
    }
    pub fn read(&self) -> Value {
        let inner: Value = (&**self.0.load()).clone();
        inner.clone()
    }
}

impl Values for PossiblyRecursive {
    fn deoptimize(&self) -> CoreValue {
        let inner: Value = (&**self.0.load()).clone();
        inner.deoptimize()
    }
    fn internal_equal(&self, _this: &Value, other: &Value) -> SKleene {
        self.read().internal_equal(other)
    }
}

pub fn run_gc() -> () {
    todo!();
}

#[derive(Debug, Clone)]
pub struct Nat(BigUint);

impl Nat {
    // todo: check endian
    pub fn to_bitbox32_le(&self) -> BitBox<Lsb0, u32> {
        BitBox::from_bitslice(self.0.to_u32_digits().view_bits::<Lsb0>())
    }
    // todo: check endian
    pub fn from_bitslice32_le(x: &BitSlice<Lsb0, u32>) -> Self {
        // todo: check as_raw_slice
        Nat(BigUint::from_slice(x.as_raw_slice()))
    }
}

impl Values for Nat {
    fn deoptimize(&self) -> CoreValue {
        CoreValue::Tagged(name::value::BINARY_LE_NAT.clone(), list!(Value::from(self.to_bitbox32_le().iter().map(|x|Value::from(*x)).collect::<Vec<Value>>())))
    }
}

#[derive(Debug, Clone)]
pub struct Char(char);

impl From<&Char> for Nat {
    fn from(x: &Char) -> Self {
        // todo: check `as u32`
        Nat(BigUint::new(vec![x.0 as u32]))
    }
}

impl Values for Char {
    fn deoptimize(&self) -> CoreValue {
        CoreValue::Tagged(name::value::CHAR.clone(), list!(Value::new(Nat::from(self))))
    }
}

#[derive(Debug, Clone)]
pub struct CharString(String);

impl Values for CharString {
    fn deoptimize(&self) -> CoreValue {
        CoreValue::Tagged(name::value::STRING.clone(), list!(Value::from(self.0.chars().map(|x|Value::new(Char(x))).collect::<Vec<_>>())))
    }
}

#[cfg(test)]
mod tests {
    #[test]
    fn it_works() {
        assert_eq!(2 + 2, 4);
    }
}
