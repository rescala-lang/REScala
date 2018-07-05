package rescala.levelbased

import rescala.core.Initializer.InitValues
import rescala.core.{AdmissionTicket, ReSource, Scheduler}
import rescala.twoversion.TwoVersionSchedulerImpl

/**
  * Basic implementations of propagation engines
  */
trait LevelBasedPropagationEngines {

  private[rescala] class SimpleNoLock extends LevelBasedPropagation[SimpleStruct] {
    override protected def makeDerivedStructState[P](ip: InitValues[P]): SimpleStruct#State[P, SimpleStruct] = {
      new LevelStateImpl(ip)
    }
    override def releasePhase(): Unit = ()
    override def preparationPhase(initialWrites: Set[ReSource[SimpleStruct]]): Unit = {}
    override def dynamicDependencyInteraction(dependency: ReSource[SimpleStruct]): Unit = {}
  }

  implicit val synchron: Scheduler[SimpleStruct] = {
    new TwoVersionSchedulerImpl[SimpleStruct, SimpleNoLock]("Synchron", _ => new SimpleNoLock) {
      override def executeTurn[R](initialWrites: Set[ReSource[SimpleStruct]], admissionPhase: AdmissionTicket[SimpleStruct] => R): R =
        synchronized { super.executeTurn(initialWrites, admissionPhase) }
    }
  }

  implicit val unmanaged: Scheduler[SimpleStruct] = new TwoVersionSchedulerImpl[SimpleStruct, SimpleNoLock]("Unmanaged", _ => new SimpleNoLock())

}

object LevelBasedPropagationEngines extends LevelBasedPropagationEngines
