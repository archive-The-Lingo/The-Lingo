use std::fmt::Debug;
use std::ops::Deref;
use std::ptr;
use std::sync::Arc;

use arc_swap::ArcSwap;
use downcast_rs::Downcast;
use downcast_rs::impl_downcast;
use trilean::SKleene;

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

type CoreIdentifier = String;

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

type Identifier = Value;

#[derive(Debug, Clone)]
pub enum Expression {
    Id(Identifier),
    Quote(Value),
    ApplyFunction(Box<Expression>, Vec<Expression>),
    ApplyMacro(Box<Expression>, Vec<Value>),
    Comment(Box<Expression>, Value),
    Builtin(ExpressionBuiltin),
}
impl Values for Expression {
    fn deoptimize(&self) -> CoreValue {
        todo!()
    }
}

#[derive(Debug, Clone)]
pub enum ExpressionBuiltin {
    IsEmptyList(Box<Expression>),
    IsSymbol(Box<Expression>),
    NewSymbol(Box<Expression>),
    ReadSymbol(Box<Expression>),
    IsNonEmptyList(Box<Expression>),
    ReadNonEmptyListHead(Box<Expression>),
    ReadNonEmptyListTail(Box<Expression>),
    IsTagged(Box<Expression>),
    ReadTaggedTag(Box<Expression>),
    ReadTaggedData(Box<Expression>),
    IsException(Box<Expression>),
    ReadExceptionTag(Box<Expression>),
    ReadExceptionData(Box<Expression>),
    Recursive(Identifier, Box<Expression>),
    Evaluate(Box<Expression>, Box<Expression>),
    Lambda(Vec<Identifier>, Option<Identifier>, Box<Expression>),
    ReadBoolean(Box<Expression>, Box<Expression>, Box<Expression>),

    // for easy using - could be implemented in the lingo itself
    IsBoolean(Box<Expression>),
    IsMapping(Box<Expression>),
    ReadMapping(Box<Expression>, Box<Expression>),
}
impl Values for ExpressionBuiltin {
    fn deoptimize(&self) -> CoreValue {
        todo!()
    }
}

#[cfg(test)]
mod tests {
    #[test]
    fn it_works() {
        assert_eq!(2 + 2, 4);
    }
}
