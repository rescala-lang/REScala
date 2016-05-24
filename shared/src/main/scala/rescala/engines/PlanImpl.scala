package rescala.engines

import rescala.graph.Struct
import rescala.propagation.AbstractPropagation

import scala.util.DynamicVariable

trait PlanImpl[S <: Struct, TImpl <: AbstractPropagation[S]] extends Engine[S, TImpl] {

  protected def makeTurn(): TImpl

  private val currentTurn: DynamicVariable[Option[TImpl]] = new DynamicVariable[Option[TImpl]](None)

  override def subplan[T](initialWrites: Reactive*)(f: TImpl => T): T = currentTurn.value match {
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
    *   - run registered observers, the turn is no longer valid but the locks are still held.
    * - run the release phase
    *   - this must always run, even in the case that something above fails. it should do cleanup and free any locks to avoid starvation.
    * - run the party! phase
    *   - not yet implemented
    * */
  override def plan[Res](initialWrites: Reactive*)(admissionPhase: TImpl => Res): Res = {

    val turn = makeTurn
    val result = try {
      val turnResult = currentTurn.withValue(Some(turn)) {
        turn.preparationPhase(initialWrites.toList)
        val admissionResult = admissionPhase(turn)
        turn.propagationPhase()
        turn.commitPhase()
        admissionResult
      }
      turnResult
    }
    catch {
      case e: Throwable =>
        turn.rollbackPhase()
        throw e
    }
    finally {
      turn.releasePhase()
    }
    turn.observerPhase()
    result
  }

}
