use async_std::sync::{Arc, RwLock, Mutex};
use std::{fmt, ops::Deref};
use futures::prelude::Future;
use async_recursion::async_recursion;
use async_trait::async_trait;
extern crate num_bigint;
extern crate num_traits;
type Nat = num_bigint::BigUint;
#[macro_use]
extern crate lazy_static;

#[derive(Debug, Clone)]
pub struct Value (Arc<RwLock<ValueUnpacked>>);
impl PartialEq for Value {
    fn eq(&self, other: &Self) -> bool {
        Arc::ptr_eq(&self.0, &other.0)
    }
}
impl Eq for Value {}
impl Value {
    fn new(x: ValueUnpacked) -> Self {
        Value(Arc::new(RwLock::new(x)))
    }
    pub async fn is_weak_head_normal_form(&self) -> bool {
        match &*self.0.read().await {
            ValueUnpacked::Just(_) | ValueUnpacked::Delay(_) => false,
            _ => true
        }
    }
    async fn get_weak_head_normal_form(&self) -> Self {
        if self.is_weak_head_normal_form().await {
            return self.clone();
        }
        let mut status_evaluating: Value = self.clone();
        let mut status_history: Vec<Value> = vec![];
        while !status_evaluating.is_weak_head_normal_form().await {
            let locked = status_evaluating.0.read().await;
            match &*locked {
                ValueUnpacked::Just(x) => {
                    status_history.push(status_evaluating.clone());
                    let next = x.clone();
                    drop(locked);
                    status_evaluating = next;
                },
                ValueUnpacked::Delay(x) => {
                    status_history.push(status_evaluating.clone());
                    let next = (&mut *x.countinue.lock().await).await;
                    drop(locked);
                    status_evaluating = next;
                },
                _ => {
                    break;
                }
            }
        }
        assert!(status_evaluating.is_weak_head_normal_form().await);
        for x in status_history.iter() {
            assert!(*x != status_evaluating);
            *x.0.write().await = ValueUnpacked::Just(status_evaluating.clone());
        }
        status_evaluating
    }
    async fn is_just(&self) -> bool {
        match &*self.0.read().await {
            ValueUnpacked::Just(_) => true,
            _ => false
        }
    }
    async fn remove_justs(&self) -> Self {
        if !self.is_just().await {
            return self.clone();
        }
        let mut status_evaluating: Value = self.clone();
        let mut status_history: Vec<Value> = vec![];
        while status_evaluating.is_just().await {
            let locked = status_evaluating.0.read().await;
            match &*locked {
                ValueUnpacked::Just(x) => {
                    status_history.push(status_evaluating.clone());
                    let next = x.clone();
                    drop(locked);
                    status_evaluating = next;
                },
                _ => {
                    break;
                }
            }
        }
        assert!(!status_evaluating.is_just().await);
        for x in status_history.iter() {
            assert!(*x != status_evaluating);
            *x.0.write().await = ValueUnpacked::Just(status_evaluating.clone());
        }
        status_evaluating
    }
    async fn forced_equal(&self, other: &Self) -> bool {
        self.clone().moved_forced_equal(other.clone()).await
    }
    #[async_recursion]
    async fn moved_forced_equal(self, other: Self) -> bool {
        if self == other { return true; }
        match (&*self.get_weak_head_normal_form().await.0.read().await,
                &*other.get_weak_head_normal_form().await.0.read().await) {
            (ValueUnpacked::Null, ValueUnpacked::Null) => true,
            (ValueUnpacked::Symbol(x), ValueUnpacked::Symbol(y)) => {
                if x == y {
                    *self.0.write().await = ValueUnpacked::Just(other.clone());
                    true
                } else {
                    false
                }
            },
            (ValueUnpacked::Pair(x0, x1), ValueUnpacked::Pair(y0, y1)) => {
                if x0.forced_equal(y0).await && x1.forced_equal(y1).await {
                    *self.0.write().await = ValueUnpacked::Just(other.clone());
                    true
                } else {
                    false
                }
            },
            (ValueUnpacked::Struct(x0, x1), ValueUnpacked::Struct(y0, y1)) => {
                if x0.forced_equal(y0).await && x1.forced_equal(y1).await {
                    *self.0.write().await = ValueUnpacked::Just(other.clone());
                    true
                } else {
                    false
                }
            },
            (ValueUnpacked::Just(_), _) | (_, ValueUnpacked::Just(_)) |
              (_, ValueUnpacked::Delay(_)) | (ValueUnpacked::Delay(_), _) => panic!("assert failed"),
            (ValueUnpacked::OptimizedWeakHeadNormalForm(x), _) => x.forced_equal(&other).await,
            (_, ValueUnpacked::OptimizedWeakHeadNormalForm(y)) => y.forced_equal(&self).await,
            (_, _) => false,
        }
    }
}

lazy_static! {
    pub static ref NULL: Value = Value::new(ValueUnpacked::Null);
}
pub fn new_symbol(x : &String) -> Value {
    Value::new(ValueUnpacked::Symbol(x.clone()))
}
pub fn new_pair(x : &Value, y : &Value) -> Value {
    Value::new(ValueUnpacked::Pair(x.clone(), y.clone()))
}
pub fn new_struct(x : &Value, y : &Value) -> Value {
    Value::new(ValueUnpacked::Struct(x.clone(), y.clone()))
}

#[derive(Debug)]
enum ValueUnpacked {
    Null,
    Symbol(String),
    Pair(Value,Value),
    Struct(Value,Value),
    Just(Value),
    Delay(ValueUnpackedDelay),
    OptimizedWeakHeadNormalForm(OptimizedWeakHeadNormalForm)
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
enum OptimizedWeakHeadNormalForm {
    Mapping(Mapping),
    Nat(Nat)
}
impl OptimizedWeakHeadNormalForm {
    async fn forced_equal(&self, other: &Value) -> bool {
        self.deoptimize().await.forced_equal(other).await
    }
}
#[async_trait]
trait ValueDeoptimize {
    async fn deoptimize(&self) -> Value;
}
#[async_trait]
impl ValueDeoptimize for OptimizedWeakHeadNormalForm {
    async fn deoptimize(&self) -> Value {
        match self {
            OptimizedWeakHeadNormalForm::Mapping(x) => x.deoptimize().await,
            OptimizedWeakHeadNormalForm::Nat(x) => x.deoptimize().await
        }
    }
}

#[async_trait]
impl ValueDeoptimize for Nat {
    async fn deoptimize(&self) -> Value {
        panic!("TODO")
    }
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
#[async_trait]
impl ValueDeoptimize for Mapping {
    async fn deoptimize(&self) -> Value {
        panic!("TODO")
    }
}
