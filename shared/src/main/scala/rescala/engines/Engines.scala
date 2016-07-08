package rescala.engines

import java.util.concurrent.locks.ReentrantLock

import rescala.graph.{LevelSporeImpl, Pulse, SimpleStruct}
import rescala.propagation.LevelBasedPropagation

/**
  * Basic implementations of propagation engines
  */
object Engines {
  private class SimpleNoLock extends LevelBasedPropagation[SimpleStruct] {
    def bud[P, R](initialValue: Pulse[P], transient: Boolean, initialIncoming: Set[R]): SimpleStruct#SporeP[P, R] =
      new LevelSporeImpl[P, R](initialValue, transient, initialIncoming)
    override def releasePhase(): Unit = ()
  }

  type SimpleEngine = Engine[SimpleStruct, LevelBasedPropagation[SimpleStruct]]


  implicit val synchron: SimpleEngine = new EngineImpl[SimpleStruct, SimpleNoLock](new SimpleNoLock()) {
    override def plan[R](i: Reactive*)(f: SimpleNoLock => R): R = synchronized(super.plan(i: _*)(f))
  }

  implicit val synchronFair: SimpleEngine = new EngineImpl[SimpleStruct, SimpleNoLock](new SimpleNoLock()) {
    val lock = new ReentrantLock(true)
    override def plan[R](i: Reactive*)(f: SimpleNoLock => R): R = {
      lock.lock()
      try {
        super.plan(i: _*)(f)
      }
      finally {lock.unlock()}
    }
  }

  implicit val unmanaged: SimpleEngine = new EngineImpl[SimpleStruct, SimpleNoLock](new SimpleNoLock())

  implicit val default: SimpleEngine = synchron

  val all: List[SimpleEngine] = List(synchron, unmanaged)


}

