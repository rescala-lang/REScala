package rescala.propagation.turns.instances

import rescala.propagation.Reactive
import rescala.propagation.turns.Turn
import rescala.propagation.turns.creation.TurnFactory

import scala.util.DynamicVariable

abstract class AbstractTurnFactory[TTurn <: AbstractTurn](makeTurn: () => TTurn) extends TurnFactory {
  val currentTurn: DynamicVariable[Option[TTurn]] = new DynamicVariable[Option[TTurn]](None)

  override def maybeDynamicTurn[T](f: (Turn) => T): T = currentTurn.value match {
    case None => newTurn(f)
    case Some(turn) => f(turn)
  }

  def acquirePreTurnLocks(turn: TTurn): Unit
  def releaseAllLocks(turn: TTurn): Unit

  override def newTurn[T](f: Turn => T): T = {
    implicit class sequentialLeftResult(result: T) { def ~< (sideEffects_! : Unit): T = result }
    val turn = makeTurn()
    try {
      currentTurn.withValue(Some(turn)) {
        f(turn) ~< {
          acquirePreTurnLocks(turn)
          turn.evaluateQueue()
          turn.commit()
        }
      } ~< turn.runAfterCommitHandlers()
    }
    finally {
      releaseAllLocks(turn)
    }
  }

  final val reactiveOrdering = new Ordering[(Int, Reactive)] {
    override def compare(x: (Int, Reactive), y: (Int, Reactive)): Int = y._1.compareTo(x._1)
  }
}
