package rescala.levelbased

import rescala.engine.{Engine, Turn, ValuePersistency}
import rescala.graph.Pulse.NoChange
import rescala.graph.{Change, Pulse, Reactive, Struct}
import rescala.twoversion.TwoVersionEngineImpl

import scala.language.existentials

/**
  * Basic implementations of propagation engines
  */
trait LevelBasedPropagationEngines {

  type TEngine = Engine[S, Turn[S]] forSome { type S <: Struct }

  private[rescala] class SimpleNoLock extends LevelBasedPropagation[SimpleStruct] {
    override protected def makeStructState[P](valuePersistency: ValuePersistency[P]): SimpleStruct#State[P, SimpleStruct] = {
      new LevelStructTypeImpl(valuePersistency.initialValue, valuePersistency.isTransient)
    }
    override def releasePhase(): Unit = ()
  }

  type SimpleEngine = Engine[SimpleStruct, LevelBasedPropagation[SimpleStruct]]


  implicit val synchron: SimpleEngine = new TwoVersionEngineImpl[SimpleStruct, SimpleNoLock]("Synchron", new SimpleNoLock()) {
    override private[rescala] def executeTurn[R](initialWrites: Traversable[Reactive], admissionPhase: SimpleNoLock => R): R = synchronized(super.executeTurn(initialWrites, admissionPhase))
  }

  implicit val unmanaged: SimpleEngine = new TwoVersionEngineImpl[SimpleStruct, SimpleNoLock]("Unmanaged", new SimpleNoLock())

}

object LevelBasedPropagationEngines extends LevelBasedPropagationEngines
