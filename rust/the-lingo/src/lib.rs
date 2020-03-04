use std::{sync::{Arc, Mutex}, fmt};
use futures::prelude::Future;

#[derive(Debug, Clone)]
pub struct Value (Arc<Mutex<ValueUnpacked>>);
impl PartialEq for Value {
    fn eq(&self, other: &Self) -> bool {
        Arc::ptr_eq(&self.0, &other.0)
    }
}
impl Eq for Value {}
impl Value {
    async fn forced_equal(&self, other: &Self) -> bool {
        panic!("TODO")
    }
}

#[derive(Debug)]
enum ValueUnpacked {
    Null,
    Symbol(String),
    Pair(Value,Value),
    Struct(Value,Value),
    Just(Value),
    Delay(ValueUnpackedDelay),
    Optimized(OptimizedValue)
}
struct ValueUnpackedDelay {
    countinue: Box<dyn Future<Output = Value> + Send + Sync>,
    stop: Box<dyn Future<Output = Value> + Send + Sync>
}
impl fmt::Debug for ValueUnpackedDelay {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "ValueUnpackedDelay {{ }}")
    }
}

#[derive(Debug)]
enum OptimizedValue {
    Mapping(Mapping)
}

#[derive(Debug)]
struct Mapping (Box<Vec<(Value, Value)>>);
impl Mapping {
    pub async fn set(&self, k: Value, v: Value) -> Self {
        let mut result = self.0.clone();
        result.push((k, v));
        Mapping(result)
    }
    pub async fn get(&self, to_find: &Value) -> Option<&Value> {
        for (k, v) in self.0.iter() {
            if to_find.forced_equal(k).await {
                return Option::Some(v);
            }
        }
        Option::None
    }
}

