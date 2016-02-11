package rescala.turns

import java.util.concurrent.locks.ReentrantLock

import rescala.graph.{SimpleSpores, Reactive, Spores}
import rescala.propagation.{AbstractPropagation, PropagationImpl}
import rescala.synchronization.{NoLocking, FactoryReference}

import scala.util.DynamicVariable

object Engines {
  type SS = SimpleSpores.type
  type NoLockEngine = Engine[SS, NoLocking[SS]]

  implicit val synchron: NoLockEngine = new Impl[SS, NoLocking[SS]](SimpleSpores, new FactoryReference(SimpleSpores) with NoLocking[SS]) {
    override def plan[R](i: Reactive*)(f: NoLocking[SS] => R): R = synchronized(super.plan(i: _*)(f))
  }

  implicit val synchronFair: NoLockEngine = new Impl[SS, NoLocking[SS]](SimpleSpores, new FactoryReference(SimpleSpores) with NoLocking[SS]) {
    val lock = new ReentrantLock(true)
    override def plan[R](i: Reactive*)(f: NoLocking[SS] => R): R = {
      lock.lock()
      try {
        super.plan(i: _*)(f)
      }
      finally {lock.unlock()}
    }
  }

  implicit val unmanaged: NoLockEngine = new Impl[SS, NoLocking[SS]](SimpleSpores, new FactoryReference(SimpleSpores) with NoLocking[SS])

  implicit val default: NoLockEngine = synchron

  val all: List[NoLockEngine] = List(synchron, unmanaged)

  class Impl[S <: Spores, TImpl <: AbstractPropagation[S]](override private[rescala] val bufferFactory: S, makeTurn: => TImpl) extends Engine[S, TImpl] {

    val currentTurn: DynamicVariable[Option[TImpl]] = new DynamicVariable[Option[TImpl]](None)

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
      try {
        val turnResult = currentTurn.withValue(Some(turn)) {
          turn.lockPhase(initialWrites.toList)
          val admissionResult = admissionPhase(turn)
          turn.propagationPhase()
          turn.commitPhase()
          admissionResult
        }
        turn.observerPhase()
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
    }

  }

}
