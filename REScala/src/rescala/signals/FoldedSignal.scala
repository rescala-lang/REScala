package rescala.signals

import rescala.events.Event
import rescala.propagation.{MaybeTurn, Turn}

class FoldedSignal[+T, +E](e: Event[E], init: T, f: (T, E) => T)(creationTurn: Turn)
  extends StaticDependentSignal[T](creationTurn) {

  //staticDependencies(Set(e))(creationTurn)

  override def initialValue()(implicit turn: Turn): T = init
  override def calculateValue()(implicit turn: Turn): T = e.pulse.toOption.fold(get)(f(get, _))

}

object FoldedSignal {
  def fold[E, T](e: Event[E], init: T, f: (T, E) => T)(implicit turn: MaybeTurn): Signal[T] = Turn.maybeTurn { turn =>
    val signal = new FoldedSignal(e, init, f)(turn)
    turn.register(signal, Set(e))
    signal
  }
}
