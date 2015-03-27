package rescala.turns

import rescala.graph.{Buffer, Reactive, STMBuffer}
import rescala.propagation.PropagationImpl
import rescala.synchronization.{EngineReference, NoLocking, STMSync, ParRP, TurnLock}

import scala.concurrent.stm.atomic
import scala.util.DynamicVariable

object Engines {

  def byName(name: String): Engine[Turn] = name match {
    case "synchron" => synchron
    case "unmanaged" => unmanaged
    case "spinningNoWait" => spinning
    case "spinningWait" => spinningWait
    case "stm" => STM
    case other => throw new IllegalArgumentException(s"unknown engine $other")
  }

  def all: List[Engine[Turn]] = List(default, STM, spinning, spinningWait, synchron, unmanaged)

  implicit val default: Engine[Turn] = spinningWithBackoff(7)

  implicit val STM: Engine[STMSync] = new Impl(new STMSync()) {
    override def plan[R](i: Reactive*)(f: STMSync => R): R = atomic { tx => super.plan(i: _*)(f) }
    override def buffer[A](default: A, commitStrategy: (A, A) => A, writeLock: TurnLock): Buffer[A] = new STMBuffer[A](default, commitStrategy)
  }

  def spinningWithBackoff(backOff: Int) = new Impl(new ParRP(backOff))

  implicit val spinning: Engine[ParRP] = new Impl(new ParRP(backOff = -1))
  implicit val spinningWait: Engine[ParRP] = new Impl(new ParRP(backOff = 0))

  implicit val synchron: Engine[NoLocking] = new Impl[NoLocking](new EngineReference(synchron) with NoLocking) {
    override def plan[R](i: Reactive*)(f: NoLocking => R): R = synchronized(super.plan(i: _*)(f))
  }
  implicit val unmanaged: Engine[NoLocking] = new Impl[NoLocking](new EngineReference(unmanaged) with NoLocking)


  class Impl[TImpl <: PropagationImpl](makeTurn: => TImpl) extends Engine[TImpl] {

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
      * - run registered observers, the turn is no longer valid but the locks are still held.
      * - run the release phase
      *   - this must aways run, even in the case that something above fails. it should do cleanup and free any locks to avoid starvation.
      * - run the party! phase
      *   - not yet implemented
      * */
    override def plan[Res](initialWrites: Reactive*)(admissionPhase: TImpl => Res): Res = {
      implicit class sequentialLeftResult[R](result: R) {def ~<(sideEffects_! : Unit): R = result }
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
      catch {
        case e: Throwable =>
          turn.rollbackPhase()
          throw e
      }
      finally {
        turn.realeasePhase()
      }
    }

  }

}
