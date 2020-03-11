use async_std::sync::{Arc, Mutex, RwLock};
use async_trait::async_trait;
use futures::{future::join_all, join, prelude::Future};
use im::{vector, vector::Vector};
use std::{fmt, pin::Pin};
extern crate num_bigint;
extern crate num_traits;
type Nat = num_bigint::BigUint;
#[macro_use]
extern crate lazy_static;
use downcast_rs::{impl_downcast, Downcast};

#[async_trait]
trait Values: Downcast + Send + Sync + fmt::Debug {
    async fn forced_equal(&self, other: &Value) -> bool;
    async fn same_form(&self, other: &Value) -> bool;
    async fn deoptimize_to_core_whnf(&self) -> ValueCoreWHNF;
    async fn evaluate(&self, map: &Mapping) -> Value;
    async fn apply(&self, args: Vector<Value>) -> Value;
}
impl_downcast!(Values);

#[derive(Debug, Clone)]
pub struct Value(Arc<RwLock<Arc<dyn Values>>>);
impl PartialEq for Value {
    fn eq(&self, other: &Self) -> bool {
        Arc::ptr_eq(&self.0, &other.0)
    }
}
impl Eq for Value {}
impl Value {
    async fn read(&self) -> Arc<dyn Values> {
        self.0.read().await.clone()
    }
    async fn unsafe_write(&self, x: Arc<dyn Values>) {
        *self.0.write().await = x;
    }
    async fn remove_pointers(&self) -> Value {
        let mut status_evaluating: Value = self.clone();
        let mut status_history: Vector<Value> = vector![];
        while let Some(x) = status_evaluating.read().await.downcast_ref::<Value>() {
            status_history.push_back(status_evaluating.clone());
            status_evaluating = x.clone();
        }
        for x in status_history.into_iter() {
            assert!(x != status_evaluating);
            x.unsafe_write(Arc::new(status_evaluating.clone())).await;
        }
        status_evaluating
    }
}
#[async_trait]
impl Values for Value {
    async fn forced_equal(&self, other: &Value) -> bool {
        self.0.read().await.clone().forced_equal(other).await
    }
    async fn same_form(&self, other: &Value) -> bool {
        self.0.read().await.clone().same_form(other).await
    }
    async fn deoptimize_to_core_whnf(&self) -> ValueCoreWHNF {
        self.0.read().await.clone().deoptimize_to_core_whnf().await
    }
    async fn evaluate(&self, map: &Mapping) -> Value {
        self.0.read().await.clone().evaluate(map).await
    }
    async fn apply(&self, args: Vector<Value>) -> Value {
        self.0.read().await.clone().apply(args).await
    }
}
#[derive(Debug, Clone)]
pub enum ValueCoreWHNF {
    Null,
    Symbol(String),
    Pair(Value, Value),
    Tagged(Value, Value),
}
#[async_trait]
impl Values for ValueCoreWHNF {
    async fn forced_equal(&self, other: &Value) -> bool {
        panic!("TODO")
    }
    async fn same_form(&self, other: &Value) -> bool {
        panic!("TODO")
    }
    async fn deoptimize_to_core_whnf(&self) -> ValueCoreWHNF {
        self.clone()
    }
    async fn evaluate(&self, map: &Mapping) -> Value {
        panic!("TODO")
    }
    async fn apply(&self, args: Vector<Value>) -> Value {
        panic!("TODO")
    }
}

#[derive(Debug, Clone)]
pub struct Mapping(Box<Vector<(Value, Value)>>);
lazy_static! {
    pub static ref NULL_MAPPING: Mapping = Mapping(Box::new(vector![]));
}