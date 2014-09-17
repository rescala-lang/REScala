package rescala.events

import rescala.propagation.{EvaluationResult, Turn}
import rescala.signals.Signal
import rescala.{Dependency, Dependant}

/** A wrapped event inside a signal, that gets "flattened" to a plain event node */
class WrappedEvent[T](wrapper: Signal[Event[T]]) extends Event[T] with Dependant {

  setDependencies(Set(wrapper, wrapper.get))

  override def reevaluate()(implicit turn: Turn): EvaluationResult = {
    for { event <- wrapper.pulse.valueOption} {
      setDependencies(Set(wrapper, event))
      pulse(event.pulse)
    }
    EvaluationResult.Done(dependants)
  }

}
