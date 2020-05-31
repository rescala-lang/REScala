package rescala.levelbased

import rescala.core.Initializer.InitValues
import rescala.core.{AdmissionTicket, ReSource, Scheduler}
import rescala.twoversion.TwoVersionScheduler

/**
  * Basic implementations of propagation engines
  */
trait LevelBasedSchedulers {

  private[rescala] class SimpleNoLock extends LevelBasedTransaction[LevelStructImpl] {
    override protected def makeDerivedStructState[P](ip: InitValues[P])
    : LevelState[P, LevelStructImpl] = {
      new LevelState(ip)
    }
    override def releasePhase(): Unit = ()
    override def preparationPhase(initialWrites: Set[ReSource[LevelStructImpl]]): Unit = {}
    override def dynamicDependencyInteraction(dependency: ReSource[LevelStructImpl]): Unit = {}
  }

  implicit val synchron: Scheduler[LevelStructImpl] = {
    new TwoVersionScheduler[LevelStructImpl, SimpleNoLock] {
      override protected def makeTurn(priorTurn: Option[SimpleNoLock]): SimpleNoLock = new SimpleNoLock
      override def schedulerName: String = "Synchron"
      override def forceNewTransaction[R](initialWrites: Set[ReSource[LevelStructImpl]], admissionPhase: AdmissionTicket[LevelStructImpl] => R): R =
        synchronized { super.forceNewTransaction(initialWrites, admissionPhase) }
    }
  }

  implicit val unmanaged: Scheduler[LevelStructImpl] =
    new TwoVersionScheduler[LevelStructImpl, SimpleNoLock] {
      override protected def makeTurn(priorTurn: Option[SimpleNoLock]): SimpleNoLock = new SimpleNoLock()
      override def schedulerName: String = "Unmanaged"
    }

}

object LevelBasedSchedulers extends LevelBasedSchedulers
