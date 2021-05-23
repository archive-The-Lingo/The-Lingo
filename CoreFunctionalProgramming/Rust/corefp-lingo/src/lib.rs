use std::fmt::Debug;
use std::ops::Deref;
use std::sync::Arc;

use arc_swap::ArcSwap;
use downcast_rs::Downcast;
use downcast_rs::impl_downcast;
use trilean::SKleene;

pub trait Values: Downcast + Debug {
    fn deoptimize(&self) -> CoreValue;
    fn internal_equal(&self, other: &Value) -> SKleene;
}
impl_downcast!(Values);

#[derive(Debug, Clone)]
pub struct Value(Arc<ArcSwap<Box<dyn Values>>>);
impl Deref for Value {
    type Target = Arc<ArcSwap<Box<dyn Values>>>;

    fn deref(&self) -> &Self::Target {
        &self.0
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
        let other = &***other.load();
        if let Some(_other) = other.downcast_ref::<CoreValue>() {
            todo!()
        } else {
            todo!()
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
