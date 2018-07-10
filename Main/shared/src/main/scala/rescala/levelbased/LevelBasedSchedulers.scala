package rescala.levelbased

import rescala.core.Initializer.InitValues
import rescala.core.{AdmissionTicket, CreationTicket, ReSource, Scheduler}
import rescala.twoversion.TwoVersionScheduler

/**
  * Basic implementations of propagation engines
  */
trait LevelBasedSchedulers {

  private[rescala] class SimpleNoLock extends LevelBasedPropagation[SimpleStruct] {
    override protected def makeDerivedStructState[P](ip: InitValues[P],
                                                     creationTicket: CreationTicket[SimpleStruct])
    : SimpleStruct#State[P, SimpleStruct] = {
      new LevelStateImpl(ip)
    }
    override def releasePhase(): Unit = ()
    override def preparationPhase(initialWrites: Set[ReSource[SimpleStruct]]): Unit = {}
    override def dynamicDependencyInteraction(dependency: ReSource[SimpleStruct]): Unit = {}
  }

  implicit val synchron: Scheduler[SimpleStruct] = {
    new TwoVersionScheduler[SimpleStruct, SimpleNoLock] {
      override protected def makeTurn(priorTurn: Option[SimpleNoLock]): SimpleNoLock = new SimpleNoLock
      override def schedulerName: String = "Synchron"
      override def executeTurn[R](initialWrites: Set[ReSource[SimpleStruct]], admissionPhase: AdmissionTicket[SimpleStruct] => R): R =
        synchronized { super.executeTurn(initialWrites, admissionPhase) }
    }
  }

  implicit val unmanaged: Scheduler[SimpleStruct] =
    new TwoVersionScheduler[SimpleStruct, SimpleNoLock] {
      override protected def makeTurn(priorTurn: Option[SimpleNoLock]): SimpleNoLock = new SimpleNoLock()
      override def schedulerName: String = "Unmanaged"
    }

}

object LevelBasedSchedulers extends LevelBasedSchedulers
