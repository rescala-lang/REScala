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

  /** goes through the whole turn lifecycle
    * - create a new turn and put it on the stack
    * - run the admission phase
    *   - this is user defined and sets source values, needs investigation of reactive creation, and read then act stuff
    * - run the locking phase
    *   - to give the turn a chance to do something before the propagation starts when it is known which reactives will change
    * - run the propagation phase
    *   - calculate the actual new value of the reactive graph
    * - run the commit phase
    *   - do cleanups on the reactives, make values permanent and so on, the turn is still valid during this phase
    * - run the after commit phase
    *   - this typically runs side effecting observers. the turn is already commited an no longer valid, but the network is still locked so this still happens in order.
    * - run the release phase
    *   - this must is aways run, even in the case that something above fails. it should do cleanup and free any locks to avoid starvation.
    * - run the party! phase
    *   - not yet implemented
    * */
  override def newTurn[T](admission: Turn => T): T = {
    implicit class sequentialLeftResult(result: T) { def ~< (sideEffects_! : Unit): T = result }
    val turn = makeTurn()
    try {
      currentTurn.withValue(Some(turn)) {
        admission(turn) ~< {
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
