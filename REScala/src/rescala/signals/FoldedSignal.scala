package rescala.signals

import rescala.{StaticDependant, DynamicDependant, Dependency}
import rescala.events.Event
import rescala.propagation.Turn

class FoldedSignal[+T, +E](e: Event[E], init: T, f: (T, E) => T)(creationTurn: Turn)
  extends DependentSignalImplementation[T](creationTurn) with StaticDependant {

  staticDependencies(Set(e))(creationTurn)

  override def initialValue()(implicit turn: Turn): T = init
  override def calculateNewValue()(implicit turn: Turn): T = e.pulse.valueOption.fold(get)(f(get, _))

}
