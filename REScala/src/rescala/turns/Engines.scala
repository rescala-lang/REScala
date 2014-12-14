package rescala.turns

import rescala.graph.Reactive
import rescala.propagation.TurnImpl
import rescala.synchronization.Pessimistic

import scala.util.DynamicVariable

object Engines {

  implicit def default: Engine[Turn] = pessimistic

  implicit val pessimistic: Engine[Turn] = new Impl(new Pessimistic()) {
    override def subplan[T](initialWrites: Reactive*)(f: (Pessimistic) => T): T = currentTurn.value match {
      case None => plan(initialWrites: _*)(f)
      case Some(turn) =>
        initialWrites.foreach(r => assert(r.lock.hasWriteAccess(turn.key), s"tried to start subplan in $turn without write access to $r"))
        f(turn)
    }
  }
  implicit val synchron: Engine[Turn] = new Impl(new TurnImpl()) {
    override def plan[T](initialWrites: Reactive*)(f: TurnImpl => T): T = synchronized(super.plan(initialWrites: _*)(f))
  }
  implicit val unmanaged: Engine[Turn] = new Impl(new TurnImpl())


  class Impl[TI <: TurnImpl](makeTurn: => TI) extends Engine[TI] {

    val currentTurn: DynamicVariable[Option[TI]] = new DynamicVariable[Option[TI]](None)

    override def subplan[T](initialWrites: Reactive*)(f: TI => T): T = currentTurn.value match {
      case None => plan(initialWrites: _*)(f)
      case Some(turn) => f(turn)
    }

    /** goes through the whole turn lifecycle
      * - create a new turn and put it on the stack
      * - run the lock phase
      *   - the turn knows which reactives will be affected and can do something before anything is really done
      * - run the admission phase
      *   - executes the user defined admission code
      * - run the propagation phase
      *   - calculate the actual new value of the reactive graph
      * - run the commit phase
      *   - do cleanups on the reactives, make values permanent and so on, the turn is still valid during this phase
      * - run the observer phase
      *   - this may have side effects as the turn is guaranteed to be finished (no rollbacks). this should still keep locks to run things in order.
      * - run the release phase
      *   - this must aways run, even in the case that something above fails. it should do cleanup and free any locks to avoid starvation.
      * - run the party! phase
      *   - not yet implemented
      * */
    override def plan[T](initialWrites: Reactive*)(admissionPhase: TI => T): T = {
      implicit class sequentialLeftResult(result: T) {def ~<(sideEffects_! : Unit): T = result }
      val turn = makeTurn
      try {
        currentTurn.withValue(Some(turn)) {
          turn.lockPhase(initialWrites.toList)
          admissionPhase(turn) ~< {
            turn.propagationPhase()
            turn.commitPhase()
          }
        } ~< turn.observerPhase()
      }
      finally {
        turn.realeasePhase()
      }
    }

  }

}