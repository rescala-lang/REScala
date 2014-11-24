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
    val turn = makeTurn()

    val result = try {
      val res = currentTurn.withValue(Some(turn)) {
        val r = f(turn)
        acquirePreTurnLocks(turn)
        turn.evaluateQueue()
        turn.commit()
        r
      }
      turn.runAfterCommitHandlers()
      res
    }
    finally {
      releaseAllLocks(turn)
    }
    result
  }

  final val reactiveOrdering = new Ordering[(Int, Reactive)] {
    override def compare(x: (Int, Reactive), y: (Int, Reactive)): Int = y._1.compareTo(x._1)
  }
}
