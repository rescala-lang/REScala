package rescala.levelbased

import rescala.core.Initializer.InitValues
import rescala.core.{ReSource, Scheduler}
import rescala.twoversion.TwoVersionSchedulerImpl

/**
  * Basic implementations of propagation engines
  */
trait LevelBasedPropagationEngines {

  private[rescala] class SimpleNoLock extends LevelBasedPropagation[SimpleStruct] {
    override protected def makeDerivedStructState[P](ip: InitValues[P]): SimpleStruct#State[P, SimpleStruct] = {
      new LevelStructTypeImpl(ip)
    }
    override def releasePhase(): Unit = ()
    override def preparationPhase(initialWrites: Traversable[ReSource[SimpleStruct]]): Unit = {}
    override def dynamicDependencyInteraction(dependency: ReSource[SimpleStruct]): Unit = {}
  }

  type SimpleEngine = Scheduler[SimpleStruct]


  implicit val synchron: SimpleEngine = {
    new TwoVersionSchedulerImpl[SimpleStruct, SimpleNoLock]("Synchron", () =>  new SimpleNoLock ) {
      override protected[rescala] def executeTurn[R](initialWrites: Traversable[ReSource], admissionPhase: AdmissionTicket => R): R =
        synchronized { super.executeTurn(initialWrites, admissionPhase) }
    }
  }

  implicit val unmanaged: SimpleEngine = new TwoVersionSchedulerImpl[SimpleStruct, SimpleNoLock]("Unmanaged", () => new SimpleNoLock())

}

object LevelBasedPropagationEngines extends LevelBasedPropagationEngines
