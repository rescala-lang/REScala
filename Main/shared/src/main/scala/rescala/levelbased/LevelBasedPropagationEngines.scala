package rescala.levelbased

import rescala.core.{Engine, ReSource, ValuePersistency}
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
    override def preparationPhase(initialWrites: Traversable[ReSource[SimpleStruct]]): Unit = {}
    override def dynamicDependencyInteraction(dependency: ReSource[SimpleStruct]): Unit = {}
  }

  type SimpleEngine = Engine[SimpleStruct]


  implicit val synchron: SimpleEngine = new TwoVersionEngineImpl[SimpleStruct, SimpleNoLock]("Synchron", new SimpleNoLock()) {
    override protected[rescala] def executeTurn[R](initialWrites: Traversable[ReSource], admissionPhase: AdmissionTicket => R): R = synchronized(super.executeTurn(initialWrites, admissionPhase))
  }

  implicit val unmanaged: SimpleEngine = new TwoVersionEngineImpl[SimpleStruct, SimpleNoLock]("Unmanaged", new SimpleNoLock())

}

object LevelBasedPropagationEngines extends LevelBasedPropagationEngines
