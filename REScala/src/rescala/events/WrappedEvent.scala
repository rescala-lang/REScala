package rescala.events

import rescala.propagation.{Turn}
import rescala.signals.Signal
import rescala.{Dependency, Dependant}

/** A wrapped event inside a signal, that gets "flattened" to a plain event node */
class WrappedEvent[T](wrapper: Signal[Event[T]]) extends EventNode[T] with Dependant {

  updateDependencies()

  private def updateDependencies() = setDependencies(Set(wrapper, wrapper.get))

  override def triggerReevaluation()(implicit turn: Turn): Unit = {
    for { event <- wrapper.pulse.valueOption} {
      turn.pulse(this, event.pulse)
    }
  }

  override def dependencyChanged[Q](dep: Dependency[Q])(implicit turn: Turn): Unit = {
    if(dep eq wrapper) updateDependencies()
    turn.addToEvalQueue(this)

  }

}
