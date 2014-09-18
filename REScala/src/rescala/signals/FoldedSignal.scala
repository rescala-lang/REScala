package rescala.signals

import rescala.events.Event
import rescala.propagation.Turn

class FoldedSignal[+T, +E](e: Event[E], init: T, f: (T, E) => T)(creationTurn: Turn)
  extends DependentSignal[T](creationTurn) {

  //staticDependencies(Set(e))(creationTurn)

  override def initialValue()(implicit turn: Turn): T = init
  override def calculateValue()(implicit turn: Turn): T = e.pulse.toOption.fold(get)(f(get, _))

}
