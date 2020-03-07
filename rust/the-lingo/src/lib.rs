//use async_recursion::async_recursion;
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

// TODO: use https://crates.io/crates/async-scoped or https://docs.rs/futures/0.3.4/futures/stream/trait.StreamExt.html#method.for_each_concurrent etc to run scoped tasks concurrently

#[derive(Debug, Clone)]
pub struct Value(Arc<RwLock<ValueUnpacked>>);
impl PartialEq for Value {
    fn eq(&self, other: &Self) -> bool {
        Arc::ptr_eq(&self.0, &other.0)
    }
}
impl Eq for Value {}
impl Value {
    pub fn new_pair(x: &Value, y: &Value) -> Value {
        Value::from(ValueUnpacked::Pair(x.clone(), y.clone()))
    }
    pub fn new_struct(x: &Value, y: &Value) -> Value {
        Value::from(ValueUnpacked::Struct(x.clone(), y.clone()))
    }
    pub async fn is_weak_head_normal_form(&self) -> bool {
        match &*self.0.read().await {
            ValueUnpacked::Just(_) | ValueUnpacked::Delay(_) => false,
            _ => true,
        }
    }
    pub async fn get_weak_head_normal_form(&self) -> Self {
        if self.is_weak_head_normal_form().await {
            return self.clone();
        }
        let mut status_evaluating: Value = self.clone();
        let mut status_history: Vector<Value> = vector![];
        loop {
            let locked = status_evaluating.0.read().await;
            match &*locked {
                ValueUnpacked::Just(x) => {
                    status_history.push_back(status_evaluating.clone());
                    let next = x.clone();
                    drop(locked);
                    status_evaluating = next;
                }
                ValueUnpacked::Delay(x) => {
                    status_history.push_back(status_evaluating.clone());
                    let next = (&mut *x.countinue.lock().await).await;
                    drop(locked);
                    status_evaluating = next;
                }
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
            _ => false,
        }
    }
    async fn remove_justs(&self) -> Self {
        if !self.is_just().await {
            return self.clone();
        }
        let mut status_evaluating: Value = self.clone();
        let mut status_history: Vector<Value> = vector![];
        loop {
            let locked = status_evaluating.0.read().await;
            match &*locked {
                ValueUnpacked::Just(x) => {
                    status_history.push_back(status_evaluating.clone());
                    let next = x.clone();
                    drop(locked);
                    status_evaluating = next;
                }
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
    pub fn evaluate(&self, env: &Mapping) -> Self {
        Value::from(ValueUnpacked::from(ValueUnpackedDelay {
            countinue: Arc::new(Mutex::new(Box::pin(self.clone().do_evaluate(env.clone())))),
            stop: Arc::new(Mutex::new(Box::pin(async { panic!("TODO") }))),
        }))
    }
    async fn do_evaluate(self, env: Mapping) -> Self {
        panic!("TODO")
    }
    pub fn apply(&self, xs: &Vector<Self>) -> Self {
        Value::from(ValueUnpacked::from(ValueUnpackedDelay {
            countinue: Arc::new(Mutex::new(Box::pin(self.clone().do_apply(xs.clone())))),
            stop: Arc::new(Mutex::new(Box::pin(async { panic!("TODO") }))),
        }))
    }
    async fn do_apply(self, xs: Vector<Self>) -> Self {
        panic!("TODO")
    }
    pub fn apply_macro(&self, env: &Mapping, xs: &Vector<Self>) -> Self {
        Value::from(ValueUnpacked::from(ValueUnpackedDelay {
            countinue: Arc::new(Mutex::new(Box::pin(
                self.clone().do_apply_macro(env.clone(), xs.clone()),
            ))),
            stop: Arc::new(Mutex::new(Box::pin(async { panic!("TODO") }))),
        }))
    }
    async fn do_apply_macro(self, env: Mapping, xs: Vector<Self>) -> Self {
        panic!("TODO")
    }
}
#[async_trait]
pub trait ValueEqual<T: 'static + Sync + Send + Clone>: Sync + Send + Clone {
    async fn moved_forced_equal(self, other: T) -> bool {
        self.forced_equal(&other).await
    }
    async fn forced_equal(&self, other: &T) -> bool {
        self.clone().moved_forced_equal(other.clone()).await
    }
    async fn moved_same_form(self, other: T) -> bool {
        self.same_form(&other).await
    }
    async fn same_form(&self, other: &T) -> bool {
        self.clone().moved_same_form(other.clone()).await
    }
}
#[async_trait]
impl ValueEqual<Value> for Value {
    async fn forced_equal(&self, other: &Value) -> bool {
        if self == other {
            return true;
        }
        let (val0, val1) = join!(
            self.get_weak_head_normal_form(),
            other.get_weak_head_normal_form()
        );
        drop(self);
        drop(other);
        match ({ val0.clone().0.read().await.clone() }, {
            val1.clone().0.read().await.clone()
        }) {
            (ValueUnpacked::Null, ValueUnpacked::Null) => {
                *val0.0.write().await = ValueUnpacked::Just(val1.clone());
                true
            }
            (ValueUnpacked::Symbol(x), ValueUnpacked::Symbol(y)) => {
                if x == y {
                    *val0.0.write().await = ValueUnpacked::Just(val1.clone());
                    true
                } else {
                    false
                }
            }
            (ValueUnpacked::Pair(x0, x1), ValueUnpacked::Pair(y0, y1)) => {
                if x0.moved_forced_equal(y0).await && x1.moved_forced_equal(y1).await {
                    *val0.0.write().await = ValueUnpacked::Just(val1.clone());
                    true
                } else {
                    false
                }
            }
            (ValueUnpacked::Struct(x0, x1), ValueUnpacked::Struct(y0, y1)) => {
                if x0.moved_forced_equal(y0).await && x1.moved_forced_equal(y1).await {
                    *val0.0.write().await = ValueUnpacked::Just(val1.clone());
                    true
                } else {
                    false
                }
            }
            (ValueUnpacked::Just(_), _)
            | (_, ValueUnpacked::Just(_))
            | (_, ValueUnpacked::Delay(_))
            | (ValueUnpacked::Delay(_), _) => panic!("assert failed"),
            (ValueUnpacked::OptimizedWeakHeadNormalForm(v0), _) => {
                if v0.moved_forced_equal(val1.clone()).await {
                    *val1.0.write().await = ValueUnpacked::Just(val0.clone());
                    true
                } else {
                    false
                }
            }
            (_, ValueUnpacked::OptimizedWeakHeadNormalForm(v1)) => {
                if v1.moved_forced_equal(val0.clone()).await {
                    *val0.0.write().await = ValueUnpacked::Just(val1.clone());
                    true
                } else {
                    false
                }
            }
            (_, _) => false,
        }
    }
    async fn same_form(&self, other: &Value) -> bool {
        if self == other {
            return true;
        }
        let (val0, val1) = join!(self.remove_justs(), other.remove_justs());
        drop(self);
        drop(other);
        match ({ val0.clone().0.read().await.clone() }, {
            val1.clone().0.read().await.clone()
        }) {
            (ValueUnpacked::Null, ValueUnpacked::Null) => {
                *val0.0.write().await = ValueUnpacked::Just(val1.clone());
                true
            }
            (ValueUnpacked::Symbol(x), ValueUnpacked::Symbol(y)) => {
                if x == y {
                    *val0.0.write().await = ValueUnpacked::Just(val1.clone());
                    true
                } else {
                    false
                }
            }
            (ValueUnpacked::Pair(x0, x1), ValueUnpacked::Pair(y0, y1)) => {
                if x0.moved_same_form(y0).await && x1.moved_same_form(y1).await {
                    *val0.0.write().await = ValueUnpacked::Just(val1.clone());
                    true
                } else {
                    false
                }
            }
            (ValueUnpacked::Struct(x0, x1), ValueUnpacked::Struct(y0, y1)) => {
                if x0.moved_same_form(y0).await && x1.moved_same_form(y1).await {
                    *val0.0.write().await = ValueUnpacked::Just(val1.clone());
                    true
                } else {
                    false
                }
            }
            (ValueUnpacked::Just(_), _) | (_, ValueUnpacked::Just(_)) => panic!("assert failed"),
            (_, ValueUnpacked::Delay(_)) | (ValueUnpacked::Delay(_), _) => false,
            (ValueUnpacked::OptimizedWeakHeadNormalForm(v0), _) => {
                if v0.moved_same_form(val1.clone()).await {
                    *val1.0.write().await = ValueUnpacked::Just(val0.clone());
                    true
                } else {
                    false
                }
            }
            (_, ValueUnpacked::OptimizedWeakHeadNormalForm(v1)) => {
                if v1.moved_same_form(val0.clone()).await {
                    *val0.0.write().await = ValueUnpacked::Just(val1.clone());
                    true
                } else {
                    false
                }
            }
            (_, _) => false,
        }
    }
}
impl From<&str> for Value {
    fn from(x: &str) -> Self {
        Value::from(ValueUnpacked::Symbol(String::from(x)))
    }
}
impl From<String> for Value {
    fn from(x: String) -> Self {
        Value::from(ValueUnpacked::Symbol(x))
    }
}
impl From<&String> for Value {
    fn from(x: &String) -> Self {
        Value::from(x.clone())
    }
}
impl From<bool> for Value {
    fn from(x: bool) -> Self {
        if x {
            TRUE_V.clone()
        } else {
            FALSE_V.clone()
        }
    }
}
impl From<ValueUnpacked> for Value {
    fn from(x: ValueUnpacked) -> Self {
        Value(Arc::new(RwLock::new(x)))
    }
}

lazy_static! {
    pub static ref EXP_S: Value = Value::from("式");
    pub static ref ID_S: Value = Value::from("標識符");
    pub static ref APPLY_FUNCTION_S: Value = Value::from("用-函式");
    pub static ref APPLY_MACRO_S: Value = Value::from("用-構式子");
    pub static ref FUNCTION_S: Value = Value::from("函式");
    pub static ref MACRO_S: Value = Value::from("構式子");
    pub static ref COMMENT_S: Value = Value::from("注釋");
    pub static ref EXCEPTION_S: Value = Value::from("異常");
    pub static ref EVALUATE_S: Value = Value::from("解算");
    pub static ref MAPPING_S: Value = Value::from("映射");
    pub static ref BUILTIN_S: Value = Value::from("內建");
    pub static ref FALSE_S: Value = Value::from("陰");
    pub static ref TRUE_S: Value = Value::from("陽");
    pub static ref CHAR_S: Value = Value::from("字符");
    pub static ref STRING_S: Value = Value::from("字串");
    pub static ref NAT_ZERO_S: Value = Value::from("自然數/零");
    pub static ref NAT_SUCC_S: Value = Value::from("自然數/加一");
    pub static ref SYMBOL_TO_STRING_S: Value = Value::from("符號→字串");
    pub static ref STRING_TO_SYMBOL_S: Value = Value::from("字串→符號");
    pub static ref CONS_PAIR_S: Value = Value::from("構造-列表/序對");
    pub static ref ELIM_PAIR_S: Value = Value::from("解構-列表/序對");
    pub static ref IS_PAIR_S: Value = Value::from("列表/序對？");
    pub static ref IS_NULL_S: Value = Value::from("列表/空？");
    pub static ref CONS_STRUCT_S: Value = Value::from("構造-結構體");
    pub static ref ELIM_STRUCT_S: Value = Value::from("解構-結構體");
    pub static ref IS_STRUCT_S: Value = Value::from("符號？");
    pub static ref RECURSION_S: Value = Value::from("遞歸");
    pub static ref NULL_V: Value = Value::from(ValueUnpacked::Null);
    pub static ref FALSE_V: Value = Value::new_struct(&FALSE_S, &NULL_V);
    pub static ref TRUE_V: Value = Value::new_struct(&TRUE_S, &NULL_V);
}

#[derive(Debug, Clone)]
enum ValueUnpacked {
    Null,
    Symbol(String),
    Pair(Value, Value),
    Struct(Value, Value),
    Just(Value),
    Delay(ValueUnpackedDelay),
    OptimizedWeakHeadNormalForm(OptimizedWeakHeadNormalForm),
}
impl From<ValueUnpackedDelay> for ValueUnpacked {
    fn from(x: ValueUnpackedDelay) -> Self {
        ValueUnpacked::Delay(x)
    }
}
impl From<OptimizedWeakHeadNormalForm> for ValueUnpacked {
    fn from(x: OptimizedWeakHeadNormalForm) -> Self {
        ValueUnpacked::OptimizedWeakHeadNormalForm(x)
    }
}
#[derive(Clone)]
struct ValueUnpackedDelay {
    countinue: Arc<Mutex<Pin<Box<dyn Future<Output = Value> + Send>>>>,
    stop: Arc<Mutex<Pin<Box<dyn Future<Output = Value> + Send>>>>,
}
impl fmt::Debug for ValueUnpackedDelay {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "ValueUnpackedDelay {{ }}")
    }
}

#[derive(Debug, Clone)]
enum OptimizedWeakHeadNormalForm {
    List(Vector<Value>),
    Mapping(Mapping),
    Nat(Nat),
    Expr(Expr),
}
#[async_trait]
impl ValueEqual<Value> for OptimizedWeakHeadNormalForm {
    async fn forced_equal(&self, other: &Value) -> bool {
        let other = other.get_weak_head_normal_form().await;
        match (self, &*other.0.read().await) {
            (
                OptimizedWeakHeadNormalForm::List(xs),
                ValueUnpacked::OptimizedWeakHeadNormalForm(OptimizedWeakHeadNormalForm::List(ys)),
            ) => return xs.forced_equal(ys).await,
            _ => (),
        }
        self.deoptimize().await.moved_forced_equal(other).await
    }
    async fn same_form(&self, other: &Value) -> bool {
        let other = other.remove_justs().await;
        match (self, &*other.0.read().await) {
            (
                OptimizedWeakHeadNormalForm::List(xs),
                ValueUnpacked::OptimizedWeakHeadNormalForm(OptimizedWeakHeadNormalForm::List(ys)),
            ) => return xs.same_form(ys).await,
            _ => (),
        }
        self.deoptimize().await.moved_same_form(other).await
    }
}
#[async_trait]
trait ValueDeoptimize: Sync + Send {
    async fn deoptimize(&self) -> Value;
}
#[async_trait]
trait ValueOptimize: Sync + Send + Sized {
    async fn optimize(arg: &Value) -> Option<Self>;
}
#[async_trait]
impl ValueDeoptimize for OptimizedWeakHeadNormalForm {
    async fn deoptimize(&self) -> Value {
        match self {
            OptimizedWeakHeadNormalForm::List(x) => x.deoptimize().await,
            OptimizedWeakHeadNormalForm::Mapping(x) => x.deoptimize().await,
            OptimizedWeakHeadNormalForm::Nat(x) => x.deoptimize().await,
            OptimizedWeakHeadNormalForm::Expr(x) => x.deoptimize().await,
        }
    }
}

impl From<Vector<Value>> for Value {
    fn from(x: Vector<Value>) -> Self {
        Value::from(ValueUnpacked::from(OptimizedWeakHeadNormalForm::List(x)))
    }
}
#[async_trait]
impl ValueEqual<Vector<Value>> for Vector<Value> {
    async fn forced_equal(&self, other: &Vector<Value>) -> bool {
        self.len() == other.len()
            && join_all(self.iter().zip(other).map(|(x, y)| x.forced_equal(y)))
                .await
                .iter()
                .fold(true, |x, y| x && *y)
    }
    async fn same_form(&self, other: &Vector<Value>) -> bool {
        self.len() == other.len()
            && join_all(self.iter().zip(other).map(|(x, y)| x.same_form(y)))
                .await
                .iter()
                .fold(true, |x, y| x && *y)
    }
}
#[async_trait]
impl ValueDeoptimize for Vector<Value> {
    async fn deoptimize(&self) -> Value {
        panic!("TODO")
    }
}
#[async_trait]
impl ValueOptimize for Vector<Value> {
    async fn optimize(arg: &Value) -> Option<Self> {
        let mut state = arg.clone();
        let mut result = vector![];
        loop {
            match &*state.get_weak_head_normal_form().await.0.read().await {
                ValueUnpacked::Null => {
                    break;
                }
                ValueUnpacked::Pair(x, xs) => {
                    result.push_back(x.clone());
                    state = xs.clone();
                }
                _ => {
                    return Option::None;
                }
            }
        }
        Option::Some(result)
    }
}

impl From<Nat> for Value {
    fn from(x: Nat) -> Self {
        Value::from(ValueUnpacked::from(OptimizedWeakHeadNormalForm::Nat(x)))
    }
}
#[async_trait]
impl ValueDeoptimize for Nat {
    async fn deoptimize(&self) -> Value {
        panic!("TODO")
    }
}

#[derive(Debug, Clone)]
pub struct Mapping(Box<Vector<(Value, Value)>>);
lazy_static! {
    pub static ref NULL_MAPPING: Mapping = Mapping(Box::new(vector![]));
    pub static ref NULL_MAPPING_V: Value = Value::from(ValueUnpacked::from(
        OptimizedWeakHeadNormalForm::Mapping((*NULL_MAPPING).clone())
    ));
}
impl Mapping {
    pub async fn new(xs: &Vector<(Value, Value)>) -> Self {
        let mut result = NULL_MAPPING.clone();
        for (k, v) in xs.iter().rev() {
            result = result.set(k, v).await;
        }
        result
    }
    pub async fn set(&self, to_set: &Value, to_set_value: &Value) -> Self {
        let mut result = vector![];
        let mut found = false;
        for (k, v) in self.0.iter() {
            if !to_set.forced_equal(k).await {
                result.push_back((k.clone(), v.clone()));
            } else {
                assert!(!found);
                found = true;
            }
        }
        result.push_back((to_set.clone(), to_set_value.clone()));
        Mapping(Box::new(result))
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
impl From<Mapping> for Vector<(Value, Value)> {
    fn from(x: Mapping) -> Self {
        (*x.0).clone()
    }
}
#[async_trait]
impl ValueDeoptimize for Mapping {
    async fn deoptimize(&self) -> Value {
        panic!("TODO")
    }
}

#[derive(Debug, Clone)]
enum Expr {
    Id(Value),
    ApplyFunction(/*function Expr*/ Value, Vector</*Expr*/ Value>),
    ApplyMacro(/*function Expr*/ Value, Vector</*Expr*/ Value>),
    Comment(/*comment Expr*/ Value, Value),
    BuiltinQuote(Value),
    BuiltinEvaluate(/*Mapping*/ Value, /*Expr*/ Value),
    BuiltinFunction(/*args*/ Value, /*Expr*/ Value),
    BuiltinApplyFunction(/*function Expr*/ Value, /*list Expr*/ Value),
    BuiltinConsStruct(/*Expr*/ Value, /*Expr*/ Value),
    BuiltinConsPair(/*Expr*/ Value, /*Expr*/ Value),
    BuiltinIsStruct(/*Expr*/ Value),
    BuiltinIsPair(/*Expr*/ Value),
    BuiltinIsNull(/*Expr*/ Value),
    BuiltinSymbolToString(/*Expr*/ Value),
    BuiltinStringToSymbol(/*Expr*/ Value),
    BuiltinRecursion(/*id Expr*/ Value, /*Expr*/ Value),
}
#[async_trait]
impl ValueDeoptimize for Expr {
    async fn deoptimize(&self) -> Value {
        panic!("TODO")
    }
}
