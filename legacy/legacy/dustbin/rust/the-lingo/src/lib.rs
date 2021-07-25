use async_trait::async_trait;
use futures::{executor::block_on, future::join_all, join, prelude::Future};
use im::{vector, vector::Vector};
use std::{
    fmt,
    iter::Iterator,
    pin::Pin,
    ptr,
    sync::{Mutex, RwLock},
};
extern crate num_bigint;
extern crate num_traits;
type Nat = num_bigint::BigUint;
#[macro_use]
extern crate lazy_static;
use gcmodule::{ThreadedObjectSpace, ThreadedCc, Trace};
use downcast_rs::{impl_downcast, Downcast};

lazy_static! {
    pub static ref THREADED_OBJECT_SPACE: ThreadedObjectSpace = ThreadedObjectSpace::default();
}
#[async_trait]
trait Values: Downcast + Trace + fmt::Debug + Send + Sync {
    async fn impl_forced_equal(&self, other: &Value) -> bool;
    async fn impl_same_form(&self, other: &Value) -> bool;
    async fn deoptimize_to_core_whnf(&self) -> Option<ValueCoreWHNF>;
    async fn deoptimize_force_to_core_whnf(&self) -> ValueCoreWHNF;
    async fn evaluate(&self, map: &Mapping) -> Value;
    async fn apply(&self, args: &Vector<Value>) -> Value;
    async fn optimize(x: &Value) -> Self
    where
        Self: Sized;
    async fn dyn_optimize_as_value(&self, x: &Value) -> Value;
}
impl_downcast!(Values);

#[derive(Trace, Debug, Clone)]
pub struct Value(ThreadedCc<RwLock<ThreadedCc<Box<dyn Values>>>>);
impl PartialEq for Value {
    fn eq(&self, other: &Self) -> bool {
        ptr::eq::<ThreadedCc<Box<dyn Values>>>(&block_on(self.read()), &block_on(other.read()))
    }
}
impl Eq for Value {}
impl Value {
    async fn read(&self) -> ThreadedCc<Box<dyn Values>> {
        self.0.borrow().read().unwrap().clone()
    }
    async fn unsafe_write(&self, x: ThreadedCc<Box<dyn Values>>) {
        *self.0.borrow().write().unwrap() = x;
    }
    async fn unsafe_write_pointer(&self, x: Value) {
        assert!(self != &x);
        self.unsafe_write(THREADED_OBJECT_SPACE.create(Box::new(x))).await;
    }
    async fn remove_pointers(&self) -> Value {
        let mut status_evaluating: Value = self.clone();
        let mut status_history: Vector<Value> = vector![];
        while let Some(x) = status_evaluating.read().await.borrow().downcast_ref::<Value>() {
            status_history.push_back(status_evaluating.clone());
            status_evaluating = x.clone();
        }
        for x in status_history.into_iter() {
            assert!(x != status_evaluating);
            x.unsafe_write_pointer(status_evaluating.clone()).await;
        }
        status_evaluating
    }
    async fn forced_equal(&self, other: &Self) -> bool {
        let this = self.remove_pointers().await;
        drop(self);
        let other = other.remove_pointers().await;
        if this == other {
            return true;
        }
        if self.read().await.borrow().impl_forced_equal(&other).await {
            other.unsafe_write_pointer(this).await;
            true
        } else {
            false
        }
    }
    async fn same_form(&self, other: &Self) -> bool {
        let this = self.remove_pointers().await;
        drop(self);
        let other = other.remove_pointers().await;
        if this == other {
            return true;
        }
        if self.read().await.borrow().impl_same_form(&other).await {
            other.unsafe_write_pointer(this).await;
            true
        } else {
            false
        }
    }
}
#[async_trait]
impl Values for Value {
    async fn impl_forced_equal(&self, other: &Value) -> bool {
        self.read().await.borrow().impl_forced_equal(other).await
    }
    async fn impl_same_form(&self, other: &Value) -> bool {
        self.read().await.borrow().impl_same_form(other).await
    }
    async fn deoptimize_force_to_core_whnf(&self) -> ValueCoreWHNF {
        self.read().await.borrow().deoptimize_force_to_core_whnf().await
    }
    async fn deoptimize_to_core_whnf(&self) -> Option<ValueCoreWHNF> {
        self.read().await.borrow().deoptimize_to_core_whnf().await
    }
    async fn optimize(x: &Value) -> Value {
        x.clone()
    }
    async fn dyn_optimize_as_value(&self, x: &Value) -> Value {
        self.read().await.borrow().dyn_optimize_as_value(x).await
    }
    async fn evaluate(&self, map: &Mapping) -> Value {
        self.read().await.borrow().evaluate(map).await
    }
    async fn apply(&self, args: &Vector<Value>) -> Value {
        self.read().await.borrow().apply(args).await
    }
}
#[derive(Debug, Clone)]
pub enum ValueCoreWHNF {
    Null,
    Symbol(String),
    Pair(Value, Value),
    Tagged(Value, Value),
}
/*
#[async_trait]
impl Values for ValueCoreWHNF {
    async fn forced_equal(&self, other: &Value) -> bool {
        match (self, &other.deoptimize_force_to_core_whnf().await) {
            (ValueCoreWHNF::Null, ValueCoreWHNF::Null) => true,
            (ValueCoreWHNF::Symbol(x), ValueCoreWHNF::Symbol(y)) => *x == *y,
            (ValueCoreWHNF::Pair(x0, x1), ValueCoreWHNF::Pair(y0, y1)) => x0.forced_equal(y0).await && x1.forced_equal(y1).await,
            (ValueCoreWHNF::Tagged(x0, x1), ValueCoreWHNF::Tagged(y0, y1)) => x0.forced_equal(y0).await && x1.forced_equal(y1).await,
            (_, _) => false,
        }
    }
    async fn same_form(&self, other: &Value) -> bool {
    }
    async fn deoptimize_force_to_core_whnf(&self) -> ValueCoreWHNF {
        self.clone()
    }
    async fn evaluate(&self, map: &Mapping) -> Value {
        panic!("TODO")
    }
    async fn apply(&self, args: Vector<Value>) -> Value {
        panic!("TODO")
    }
    async fn is_whnf(&self) -> bool {
        true
    }
}
*/

#[derive(Debug, Clone)]
pub struct Mapping(Box<Vector<(Value, Value)>>);
