package rescala.levelbased

import rescala.core.{Engine, Reactive, ValuePersistency}
import rescala.twoversion.TwoVersionEngineImpl

/**
  * Basic implementations of propagation engines
  */
trait LevelBasedPropagationEngines {

  private[rescala] class SimpleNoLock extends LevelBasedPropagation[SimpleStruct] {
    override protected def makeStructState[P](valuePersistency: ValuePersistency[P]): SimpleStruct#State[P, SimpleStruct] = {
      new LevelStructTypeImpl(valuePersistency.initialValue, valuePersistency.isTransient)
    }
    override def releasePhase(): Unit = ()
    override def dynamicDependencyInteraction(dependency: Reactive[SimpleStruct]): Unit = {}
    override def preparationPhase(initialWrites: Traversable[Reactive[SimpleStruct]]): Unit = {}
  }

  type SimpleEngine = Engine[SimpleStruct]


  implicit val synchron: SimpleEngine = new TwoVersionEngineImpl[SimpleStruct, SimpleNoLock]("Synchron", new SimpleNoLock()) {
    override protected[rescala] def executeTurn[R](initialWrites: Traversable[Reactive], admissionPhase: AdmissionTicket => R): R = synchronized(super.executeTurn(initialWrites, admissionPhase))
  }

  implicit val unmanaged: SimpleEngine = new TwoVersionEngineImpl[SimpleStruct, SimpleNoLock]("Unmanaged", new SimpleNoLock())

}

object LevelBasedPropagationEngines extends LevelBasedPropagationEngines
