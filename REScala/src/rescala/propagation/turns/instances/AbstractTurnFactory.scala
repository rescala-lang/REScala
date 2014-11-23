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
    val result = currentTurn.withValue(Some(turn)) {
      val res = f(turn)
      acquirePreTurnLocks(turn)
      try {
        turn.evaluateQueue()
        turn.commit()
      } finally {
        releaseAllLocks(turn)
      }
      res
    }
    turn.runAfterCommitHandlers()
    result
  }

  final val reactiveOrdering = new Ordering[(Int, Reactive)] {
    override def compare(x: (Int, Reactive), y: (Int, Reactive)): Int = y._1.compareTo(x._1)
  }
}
