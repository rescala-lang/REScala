package rescala.levelbased

import rescala.core.{Scheduler, ReSource, ValuePersistency}
import rescala.twoversion.TwoVersionSchedulerImpl

/**
  * Basic implementations of propagation engines
  */
trait LevelBasedPropagationEngines {

  private[rescala] class SimpleNoLock extends LevelBasedPropagation[SimpleStruct] {
    override protected def makeDerivedStructState[P](valuePersistency: ValuePersistency[P]): SimpleStruct#State[P, SimpleStruct] = {
      new LevelStructTypeImpl(valuePersistency.initialValue, valuePersistency.isTransient)
    }
    override def releasePhase(): Unit = ()
    override def preparationPhase(initialWrites: Traversable[ReSource[SimpleStruct]]): Unit = {}
    override def dynamicDependencyInteraction(dependency: ReSource[SimpleStruct]): Unit = {}
  }

  type SimpleEngine = Scheduler[SimpleStruct]


  implicit val synchron: SimpleEngine = {
    val synchronTurn = new SimpleNoLock
    new TwoVersionSchedulerImpl[SimpleStruct, SimpleNoLock]("Synchron", () =>  synchronTurn ) {
      override protected[rescala] def executeTurn[R](initialWrites: Traversable[ReSource], admissionPhase: AdmissionTicket => R): R =
        synchronized {
          try super.executeTurn(initialWrites, admissionPhase)
          finally synchronTurn.clear()
        }
    }
  }

  implicit val unmanaged: SimpleEngine = new TwoVersionSchedulerImpl[SimpleStruct, SimpleNoLock]("Unmanaged", () => new SimpleNoLock())

}

object LevelBasedPropagationEngines extends LevelBasedPropagationEngines
