use std::ptr;
use std::fmt::Debug;
use std::ops::Deref;
use std::path::Path;
use std::sync::{Arc, Mutex, Weak};

use arc_swap::ArcSwap;
use downcast_rs::Downcast;
use downcast_rs::impl_downcast;
use lazy_static::lazy_static;
use trilean::SKleene;
use weak_table::PtrWeakHashSet;

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
        self.load().deoptimize()
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

pub type Position = UNIXFilePosition;

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

impl Expression {
    pub fn evaluate(&self, environment: Mapping) -> Value {
        self.evaluate_with_option_stack(environment, None)
    }
    pub fn evaluate_with_option_stack(&self, _environment: Mapping, _stack: Option<DebugStack>) -> Value {
        match self {
            Expression::Id(_) => todo!(),
            Expression::Quote(x) => x.clone(),
            Expression::ApplyFunction(_, _) => todo!(),
            Expression::ApplyMacro(_, _) => todo!(),
            Expression::Comment(_, _) => todo!(),
            Expression::Builtin(_) => todo!(),
            Expression::Positioned(_, _) => todo!(),
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
    IsTagged(Arc<Expression>),
    ReadTaggedTag(Arc<Expression>),
    ReadTaggedData(Arc<Expression>),
    IsException(Arc<Expression>),
    ReadExceptionTag(Arc<Expression>),
    ReadExceptionData(Arc<Expression>),
    Recursive(Identifier, Arc<Expression>),
    Evaluate(Arc<Expression>, Arc<Expression>),
    Lambda(Vec<Identifier>, Option<Identifier>, Arc<Expression>),
    ReadBoolean(Arc<Expression>, Arc<Expression>, Arc<Expression>),

    // for easy using - could be implemented in the lingo itself
    IsBoolean(Arc<Expression>),
    IsMapping(Arc<Expression>),
    ReadMapping(Arc<Expression>, Arc<Expression>),
}

impl ExpressionBuiltin {
    pub fn evaluate(&self, environment: Mapping) -> Value {
        self.evaluate_with_option_stack(environment, None)
    }
    pub fn evaluate_with_option_stack(&self, _environment: Mapping, _stack: Option<DebugStack>) -> Value {
        match self {
            ExpressionBuiltin::IsEmptyList(_) => todo!(),
            ExpressionBuiltin::IsSymbol(_) => todo!(),
            ExpressionBuiltin::NewSymbol(_) => todo!(),
            ExpressionBuiltin::ReadSymbol(_) => todo!(),
            ExpressionBuiltin::IsNonEmptyList(_) => todo!(),
            ExpressionBuiltin::ReadNonEmptyListHead(_) => todo!(),
            ExpressionBuiltin::ReadNonEmptyListTail(_) => todo!(),
            ExpressionBuiltin::IsTagged(_) => todo!(),
            ExpressionBuiltin::ReadTaggedTag(_) => todo!(),
            ExpressionBuiltin::ReadTaggedData(_) => todo!(),
            ExpressionBuiltin::IsException(_) => todo!(),
            ExpressionBuiltin::ReadExceptionTag(_) => todo!(),
            ExpressionBuiltin::ReadExceptionData(_) => todo!(),
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

#[derive(Debug, Clone)]
pub struct DebugStack(Arc<ArcLinkedList<Position>>);

#[derive(Debug, Clone)]
pub enum ArcLinkedList<T> {
    Empty,
    NonEmpty(T, Arc<ArcLinkedList<T>>),
}

lazy_static! {
    static ref POSSIBLY_RECURSIVE_SET: Mutex<PtrWeakHashSet<WeakValue>> = Mutex::new(PtrWeakHashSet::new());
}

#[derive(Debug, Clone)]
pub struct PossiblyRecursive(ValueInternal);

impl PossiblyRecursive {
    pub fn new(x: &Value) -> Self {
        POSSIBLY_RECURSIVE_SET.lock().unwrap().insert((**x).clone());
        PossiblyRecursive((**x).clone())
    }
    pub fn read(&self) -> Value {
        Value(self.0.clone())
    }
}

impl Values for PossiblyRecursive {
    fn deoptimize(&self) -> CoreValue {
        self.0.deoptimize()
    }
    fn internal_equal(&self, _this: &Value, other: &Value) -> SKleene {
        self.read().internal_equal(other)
    }
}

pub fn run_gc() -> () {
    todo!();
}

#[cfg(test)]
mod tests {
    #[test]
    fn it_works() {
        assert_eq!(2 + 2, 4);
    }
}
