use std::sync::Arc;
use std::hash::{Hash, Hasher};

#[cfg(test)]
mod tests {
    #[test]
    fn it_works() {
        assert_eq!(2 + 2, 4);
    }
}

pub type CoreIdentifier = String;

// However Hole is not allowed here
pub type Identifier = Value;

#[derive(Debug, Clone)]
pub enum CoreValue {
    EmptyList,
    Symbol(CoreIdentifier),
    NonEmptyList(Value, Value),
    Tagged(Value, Value),
    Exception(Value, Value),
    Relation(Relation),
}

#[derive(Debug, Clone)]
pub struct Relation {
    outer_environment: Mapping,
    // mutual recursion
    inner: Arc<Vec<(Identifier, PartialRelation)>>,
    this: Identifier,
}

#[derive(Debug, Clone)]
pub struct PartialRelation {
    args: Vec<Identifier>,
    tail: Option<Identifier>,
    body: Value,
}

pub type Value = Arc<ValueInternal>;

#[derive(Debug, Clone)]
pub enum ValueInternal {
    CoreValue(CoreValue),
    Hole(Arc<Hole>),
    // Id is only allowed in Relation
    Id(Identifier),
}

#[derive(Debug, Clone)]
pub struct Hole {
    name: Option<CoreIdentifier>,
    id: u128,
}

impl PartialEq for Hole {
    fn eq(&self, other: &Self) -> bool {
        self.name == other.name
    }
}

impl Eq for Hole {}

impl Hash for Hole {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.id.hash(state);
    }
}

pub type Goal = Arc<GoalInternal>;

#[derive(Debug, Clone)]
pub enum GoalInternal {
    // Id is only allowed in Relation
    Id(Identifier),
    And(Goal, Goal),
    Or(Goal, Goal),
    Not(Goal, Goal),
    Eq(Value, Value),
    NotEq(Value, Value),
    Symbol(Value),
    NotSymbol(Value),
    // First Value must be a relation here.
    Apply(Value, Vec<Value>),
}

#[derive(Debug, Clone)]
pub struct World {
    goals: Vec<Goal>,
    // todo
    equals: Vec<()>,
}

pub type Universe = Vec<World>;

#[derive(Debug, Clone)]
pub enum ArcLinkedList<T> {
    Empty,
    NonEmpty(T, Arc<ArcLinkedList<T>>),
}

// TODO: optimize this.
#[derive(Debug, Clone)]
pub struct Mapping(Arc<ArcLinkedList<(Value, Value)>>);

