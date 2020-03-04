use async_std::sync::{Arc, RwLock, Mutex};
use std::fmt;
use futures::prelude::Future;
use async_recursion::async_recursion;

#[derive(Debug, Clone)]
pub struct Value (Arc<RwLock<ValueUnpacked>>);
impl PartialEq for Value {
    fn eq(&self, other: &Self) -> bool {
        Arc::ptr_eq(&self.0, &other.0)
    }
}
impl Eq for Value {}
impl Value {
    async fn not_weak_head_normal_form(&self) -> bool {
        match &*self.0.read().await {
            ValueUnpacked::Just(_) | ValueUnpacked::Delay(_) => true,
            _ => false
        }
    }
    async fn is_weak_head_normal_form(&self) -> bool {
        match &*self.0.read().await {
            ValueUnpacked::Just(_) | ValueUnpacked::Delay(_) => false,
            _ => true
        }
    }
    #[async_recursion]
    async fn get_weak_head_normal_form(self) -> Value {
        let locked = self.0.read().await;
        match &*locked {
            ValueUnpacked::Just(x) => {
                if x.is_weak_head_normal_form().await {
                    let result: Value = x.clone().get_weak_head_normal_form().await;
                    drop(locked);
                    *self.0.write().await = ValueUnpacked::Just(result.clone());
                    result
                } else {
                    x.clone()
                }
            },
            ValueUnpacked::Delay(x) => {
                let result: Value = (&mut *x.countinue.lock().await).await
                    .get_weak_head_normal_form().await;
                drop(locked);
                *self.0.write().await = ValueUnpacked::Just(result.clone());
                result
            },
            _ => panic!("TODO")
        }
    }
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
    countinue: Box<Mutex<dyn Future<Output = Value> + Unpin + Send + Sync>>,
    stop: Box<Mutex<dyn Future<Output = Value> + Unpin + Send + Sync>>
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
