package rescala.turns

import java.util.concurrent.atomic.AtomicInteger

import rescala.propagation.TurnImpl
import rescala.synchronization.{Pessimistic}

import scala.util.DynamicVariable

object Engines {

  implicit def default: Engine = pessimistic

  implicit val pessimistic: Engine = new Impl(new Pessimistic())
  implicit val synchron: Engine = new Impl(new TurnImpl()) {
    override def plan[T](f: Turn => T): T = synchronized(super.plan(f))
  }
  implicit val unmanaged: Engine = new Impl(new TurnImpl())

  val currentTurn: DynamicVariable[Option[TurnImpl]] = new DynamicVariable[Option[TurnImpl]](None)

  class Impl(makeTurn: => TurnImpl) extends Engine {

    override def subplan[T](f: (Turn) => T): T = currentTurn.value match {
      case None => plan(f)
      case Some(turn) => f(turn)
    }

    /** goes through the whole turn lifecycle
      * - create a new turn and put it on the stack
      *   - run the user code inside that turn to know the initial reactives
      * - run the lock phase
      *   - the turn knows which reactives will be affected and can do something before anything is realy done
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
    override def plan[T](generateAdmissions: Turn => T): T = {
      implicit class sequentialLeftResult(result: T) {def ~<(sideEffects_! : Unit): T = result }
      val turn = makeTurn
      try {
        currentTurn.withValue(Some(turn)) {
          generateAdmissions(turn) ~< {
            turn.lockPhase()
            turn.admissionPhase()
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