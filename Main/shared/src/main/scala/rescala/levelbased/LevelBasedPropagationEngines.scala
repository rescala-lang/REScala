package rescala.levelbased

import java.util.concurrent.locks.ReentrantLock

import rescala.engine.Engine
import rescala.graph.{Pulse, Struct}
import rescala.propagation.Turn
import rescala.twoversion.EngineImpl

import scala.language.existentials

/**
  * Basic implementations of propagation engines
  */
trait LevelBasedPropagationEngines {

  type TEngine = Engine[S, Turn[S]] forSome { type S <: Struct }


  private[rescala] class SimpleNoLock extends LevelBasedPropagation[SimpleStruct] {
    def makeStructState[P, R](initialValue: Pulse[P], transient: Boolean, initialIncoming: Set[R]): SimpleStruct#StructType[P, R] =
      new LevelStructTypeImpl[P, R](initialValue, transient, initialIncoming)
    override def releasePhase(): Unit = ()
  }

  type SimpleEngine = Engine[SimpleStruct, LevelBasedPropagation[SimpleStruct]]


  implicit val synchron: SimpleEngine = new EngineImpl[SimpleStruct, SimpleNoLock]("Synchron", new SimpleNoLock()) {
    override def plan[R](i: Reactive*)(f: SimpleNoLock => R): R = synchronized(super.plan(i: _*)(f))
  }

  implicit val synchronFair: SimpleEngine = new EngineImpl[SimpleStruct, SimpleNoLock]("SynchronFair", new SimpleNoLock()) {
    val lock = new ReentrantLock(true)
    override def plan[R](i: Reactive*)(f: SimpleNoLock => R): R = {
      lock.lock()
      try {
        super.plan(i: _*)(f)
      }
      finally {lock.unlock()}
    }
  }

  implicit val unmanaged: SimpleEngine = new EngineImpl[SimpleStruct, SimpleNoLock]("Unmanaged", new SimpleNoLock())

}

object LevelBasedPropagationEngines extends LevelBasedPropagationEngines
