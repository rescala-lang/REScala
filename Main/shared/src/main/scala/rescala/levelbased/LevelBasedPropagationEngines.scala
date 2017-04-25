package rescala.levelbased

import rescala.engine.{Engine, Turn}
import rescala.graph.{Reactive, Struct}
import rescala.twoversion.TwoVersionEngineImpl

import scala.language.existentials

/**
  * Basic implementations of propagation engines
  */
trait LevelBasedPropagationEngines {

  type TEngine = Engine[S, Turn[S]] forSome { type S <: Struct }

  private[rescala] class SimpleNoLock extends LevelBasedPropagation[SimpleStruct] {
    override private[rescala] def makeStructState[P](initialValue: P, transient: Boolean, initialIncoming: Set[Reactive[SimpleStruct]], hasState: Boolean): SimpleStruct#State[P, SimpleStruct] = {
      new LevelStructTypeImpl[P, SimpleStruct](initialValue, transient, initialIncoming)
    }
    override def releasePhase(): Unit = ()
  }

  type SimpleEngine = Engine[SimpleStruct, LevelBasedPropagation[SimpleStruct]]


  implicit val synchron: SimpleEngine = new TwoVersionEngineImpl[SimpleStruct, SimpleNoLock]("Synchron", new SimpleNoLock()) {
    override def transaction[R](i: Reactive*)(f: SimpleNoLock => R): R = synchronized(super.transaction(i: _*)(f))
  }

  implicit val unmanaged: SimpleEngine = new TwoVersionEngineImpl[SimpleStruct, SimpleNoLock]("Unmanaged", new SimpleNoLock())

}

object LevelBasedPropagationEngines extends LevelBasedPropagationEngines
