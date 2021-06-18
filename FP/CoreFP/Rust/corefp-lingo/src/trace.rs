use std::any::Any;
/*
pub trait TraceSpecific<Tracer> {
    fn trace_specific(&self, tracer: &mut Tracer);
}

impl<Tracer: 'static, T: Trace> TraceSpecific<Tracer> for T {
    fn trace_specific(&self, tracer: &mut Tracer) {
        self.trace(tracer);
    }
}
*/
pub trait TraceSpecificT {
    type Tracer : 'static;
    fn trace_specific_t(&self, tracer: &mut Self::Tracer);
}

pub trait Trace {
    fn trace(&self, tracer: &mut dyn Any);
}

impl<T: TraceSpecificT> Trace for T {
    fn trace(&self, tracer: &mut dyn Any) {
        if let Some(tracer) = tracer.downcast_mut::<T::Tracer>() {
            self.trace_specific_t(tracer);
        }
    }
}
/*
impl<T: TraceSpecificT> TraceSpecific<T::Tracer> for T {
    fn trace_specific(&self, tracer: &mut T::Tracer) {
        self.trace_specific_t(tracer)
    }
}
*/
/*
pub trait TraceGeneric {
    fn trace_generic<Tracer: 'static>(&self, tracer: &mut Tracer);
}

impl<Tracer: 'static, T: TraceGeneric> TraceSpecific<Tracer> for T {
    fn trace_specific(&self, tracer: &mut Tracer) {
        self.trace(tracer);
    }
}

impl<T:Trace> TraceGeneric for T {
    fn trace_generic<Tracer: 'static>(&self, tracer: &mut Tracer){
        self.trace(tracer);
    }
}

impl<T:TraceGeneric> Trace for T {
    fn trace(&self, tracer: &mut dyn Any) {
        self.trace_generic(tracer);
    }
}
*/
