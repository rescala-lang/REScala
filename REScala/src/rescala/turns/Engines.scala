package rescala.turns

import rescala.graph.{STMBuffer, Buffer, Reactive}
import rescala.propagation.TurnImpl
import rescala.synchronization.{NothingSpecial, EngineReference, Pessimistic, TurnLock, STMSync, SpinningInitPessimistic, Prelock}

import scala.util.DynamicVariable
import scala.concurrent.stm.atomic

object Engines {


  def byName(name: String): Engine[Turn] = name match {
    case "pessimistic" => pessimistic
    case "synchron" => synchron
    case "unmanaged" => unmanaged
    case "spinningInit" => spinningInit
    case "stm" => STM
    case _ => default
  }

  implicit def default: Engine[Turn] = pessimistic

  implicit val STM: Engine[STMSync] = new Impl(new STMSync()) {
    override def plan[T1, T2](i: Reactive*)(f: STMSync => T1)(g: (STMSync, T1) => T2): T2 = atomic { tx => super.plan(i: _*)(f)(g) }
    override def buffer[A](default: A, commitStrategy: (A, A) => A, writeLock: TurnLock): Buffer[A] = new STMBuffer[A](default, commitStrategy)
  }

  implicit val spinningInit: Engine[SpinningInitPessimistic] = new Impl(new SpinningInitPessimistic())

  implicit val pessimistic: Engine[Pessimistic] = new Impl(new Pessimistic()) {
    override def subplan[T](initialWrites: Reactive*)(f: (Pessimistic) => T): T = currentTurn.value match {
      case None => planned(initialWrites: _*)(f)
      case Some(turn) =>
        initialWrites.foreach(r => assert(r.lock.hasWriteAccess(turn.key), s"tried to start subplan in $turn without write access to $r"))
        f(turn)
    }
  }
  implicit val synchron: Engine[NothingSpecial] = new Impl[NothingSpecial](new EngineReference(synchron) with NothingSpecial) {
    override def plan[T1, T2](i: Reactive*)(f: NothingSpecial => T1)(g: (NothingSpecial, T1) => T2): T2 = synchronized(super.plan(i: _*)(f)(g))
  }
  implicit val unmanaged: Engine[NothingSpecial] = new Impl[NothingSpecial](new EngineReference(unmanaged) with NothingSpecial)


  class Impl[TI <: TurnImpl](makeTurn: => TI) extends Engine[TI] {

    val currentTurn: DynamicVariable[Option[TI]] = new DynamicVariable[Option[TI]](None)

    override def subplan[T](initialWrites: Reactive*)(f: TI => T): T = currentTurn.value match {
      case None => planned(initialWrites: _*)(f)
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
    override def plan[T1, T2](initialWrites: Reactive*)(admissionPhase: TI => T1)(checkPhase: (TI, T1) => T2): T2 = {
      implicit class sequentialLeftResult[R](result: R) {def ~<(sideEffects_! : Unit): R = result }
      val turn = makeTurn
      try {
        currentTurn.withValue(Some(turn)) {
          turn.lockPhase(initialWrites.toList)
          val res = admissionPhase(turn)
          turn.propagationPhase()
          turn.commitPhase()
          checkPhase(turn, res)
        } ~< turn.observerPhase()
      }
      finally {
        turn.realeasePhase()
      }
    }

  }

}