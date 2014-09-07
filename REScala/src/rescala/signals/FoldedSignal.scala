package rescala.signals

import rescala.Dependency
import rescala.events.Event
import rescala.propagation.Turn

class FoldedSignal[+T, +E](e: Event[E], init: T, f: (T, E) => T)(creationTurn: Turn)
  extends DependentSignalImplementation[T](creationTurn) {

  addDependency(e)

  override def initialValue()(implicit turn: Turn): T = init
  override def calculateNewValue()(implicit turn: Turn): T = e.pulse.valueOption.fold(get)(f(get, _))

}
