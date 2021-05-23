use arc_swap::ArcSwap;
use std::sync::Arc;
use downcast_rs::Downcast;
use std::fmt::Debug;
use trilean::SKleene;

pub trait Values: Downcast + Debug {
    fn deoptimize(&self) -> CoreValue;
    fn internal_equal(&self, other: &Value) -> SKleene;
}

#[derive(Trace, Debug, Clone)]
pub struct Value(Arc<ArcSwap<dyn Values>>);

pub enum CoreValue {
    EmptyList,
    Symbol(String),
    NonEmptyList(Value, Value),
    Tagged(Value, Value),
    Exception(Value, Value),
}


#[cfg(test)]
mod tests {
    #[test]
    fn it_works() {
        assert_eq!(2 + 2, 4);
    }
}
