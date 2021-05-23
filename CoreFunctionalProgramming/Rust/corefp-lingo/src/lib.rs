use std::fmt::Debug;
use std::ops::Deref;
use std::sync::Arc;

use arc_swap::ArcSwap;
use downcast_rs::Downcast;
use downcast_rs::impl_downcast;
use trilean::SKleene;
use std::ptr;

pub trait Values: Downcast + Debug {
    fn deoptimize(&self) -> CoreValue;
    fn internal_equal(&self, _other: &Value) -> SKleene {
        SKleene::Unknown
    }
    fn equal(&self, other: &Value) -> bool {
        match self.internal_equal(other) {
            SKleene::False => false,
            SKleene::True => true,
            SKleene::Unknown => self.deoptimize().core_equal(&other.deoptimize()),
        }
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
        self.0.equal(other)
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

impl Values for OptimizableValue {
    fn deoptimize(&self) -> CoreValue {
        self.load().deoptimize()
    }

    fn internal_equal(&self, other: &Value) -> SKleene {
        self.load().internal_equal(other)
    }
}

#[derive(Debug, Clone)]
pub enum CoreValue {
    EmptyList,
    Symbol(String),
    NonEmptyList(Value, Value),
    Tagged(Value, Value),
    Exception(Value, Value),
}

impl Values for CoreValue {
    fn deoptimize(&self) -> CoreValue {
        self.clone()
    }
    fn internal_equal(&self, other: &Value) -> SKleene {
        if let Some(_other) = other.downcast_ref::<CoreValue>() {
            todo!()
        } else {
            todo!()
        }
    }
}

impl CoreValue {
    pub fn core_equal(&self, other: &CoreValue) -> bool {
        match (self, other) {
            (CoreValue::EmptyList, CoreValue::EmptyList) => true,
            (CoreValue::Symbol(x), CoreValue::Symbol(y)) => x == y,
            (CoreValue::NonEmptyList(x0,y0),CoreValue::NonEmptyList(x1,y1)) => x0.equal(x1) && y0.equal(y1),
            (CoreValue::Tagged(x0,y0),CoreValue::Tagged(x1,y1)) => x0.equal(x1) && y0.equal(y1),
            (CoreValue::Exception(x0,y0),CoreValue::Exception(x1,y1)) => x0.equal(x1) && y0.equal(y1),
            (_, _) => false,
        }
    }
}

#[cfg(test)]
mod tests {
    #[test]
    fn it_works() {
        assert_eq!(2 + 2, 4);
    }
}
