use std::fmt::Debug;
use std::ops::Deref;
use std::ptr;
use std::sync::Arc;

use arc_swap::ArcSwap;
use downcast_rs::Downcast;
use downcast_rs::impl_downcast;
use trilean::SKleene;
use std::path::Path;

pub trait Values: Downcast + Debug {
    fn deoptimize(&self) -> CoreValue;
    fn internal_equal(&self, _this: &Value, _other: &Value) -> SKleene {
        SKleene::Unknown
    }
}
impl_downcast!(Values);

#[derive(Debug, Clone)]
pub struct Value(Arc<dyn Values>);

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
    Positioned(Arc<Expression>, UNIXFilePosition),
}

#[derive(Debug, Clone)]
pub struct UNIXFilePosition {
    file: Box<Path>,
    line: u128,
    column: u128,
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

impl Values for ExpressionBuiltin {
    fn deoptimize(&self) -> CoreValue {
        todo!()
    }
}

pub struct PossiblyRecursive(Value);

impl PossiblyRecursive {
    pub fn new(_x: Value) -> PossiblyRecursive {
        todo!();
    }
}

pub fn run_gc() {
    todo!();
}

#[cfg(test)]
mod tests {
    #[test]
    fn it_works() {
        assert_eq!(2 + 2, 4);
    }
}
