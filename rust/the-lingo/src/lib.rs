use std::{sync::{Arc, Mutex}, fmt};
use futures::prelude::Future;

#[derive(Debug)]
pub struct Value (Arc<Mutex<ValueUnpacked>>);

#[derive(Debug)]
enum ValueUnpacked {
    Null,
    Symbol(String),
    Pair(Value,Value),
    Struct(Value,Value),
    Just(Value),
    Delay(Box<ValueUnpackedDelay>)
}

struct ValueUnpackedDelay {
    countinue: dyn Future<Output = Value> + Send + Sync
}
impl fmt::Debug for ValueUnpackedDelay {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "ValueUnpackedDelay {{ }}")
    }
}
