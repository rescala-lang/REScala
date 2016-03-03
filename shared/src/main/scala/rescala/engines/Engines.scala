package rescala.engines

import java.util.concurrent.locks.ReentrantLock

import rescala.graph.{Struct, SimpleStruct}
import rescala.propagation.{LevelBasedPropagation, Turn, NoLocking}

object Engines {
  type SS = SimpleStruct.type
  type SimpleEngine = Engine[SS, LevelBasedPropagation[SS]]

  private class SimpleNoLock extends LevelBasedPropagation[SS] {
    override val bufferFactory = SimpleStruct
    override def releasePhase(): Unit = ()
  }


  implicit val synchron: SimpleEngine = new EngineImpl[SS, SimpleNoLock](new SimpleNoLock()) {
    override def plan[R](i: Reactive*)(f: SimpleNoLock => R): R = synchronized(super.plan(i: _*)(f))
  }

  implicit val synchronFair: SimpleEngine = new EngineImpl[SS, SimpleNoLock](new SimpleNoLock()) {
    val lock = new ReentrantLock(true)
    override def plan[R](i: Reactive*)(f: SimpleNoLock => R): R = {
      lock.lock()
      try {
        super.plan(i: _*)(f)
      }
      finally {lock.unlock()}
    }
  }

  implicit val unmanaged: SimpleEngine = new EngineImpl[SS, SimpleNoLock](new SimpleNoLock())

  implicit val default: SimpleEngine = synchron

  val all: List[SimpleEngine] = List(synchron, unmanaged)


}

