package rescala.engines

import java.util.concurrent.locks.ReentrantLock

import rescala.graph.{Struct, SimpleStruct}
import rescala.propagation.{Turn, NoLocking}

object Engines {
  type SS = SimpleStruct.type
  type NoLockEngine = Engine[SS, NoLocking[SS]]

  private class SimpleNoLock[S <: Struct] extends NoLocking[SS] {
    override val bufferFactory = SimpleStruct
  }


  implicit val synchron: NoLockEngine = new EngineImpl[SS, NoLocking[SS]](SimpleStruct, new SimpleNoLock()) {
    override def plan[R](i: Reactive*)(f: NoLocking[SS] => R): R = synchronized(super.plan(i: _*)(f))
  }

  implicit val synchronFair: NoLockEngine = new EngineImpl[SS, NoLocking[SS]](SimpleStruct, new SimpleNoLock()) {
    val lock = new ReentrantLock(true)
    override def plan[R](i: Reactive*)(f: NoLocking[SS] => R): R = {
      lock.lock()
      try {
        super.plan(i: _*)(f)
      }
      finally {lock.unlock()}
    }
  }

  implicit val unmanaged: NoLockEngine = new EngineImpl[SS, NoLocking[SS]](SimpleStruct, new SimpleNoLock())

  implicit val default: NoLockEngine = synchron

  val all: List[NoLockEngine] = List(synchron, unmanaged)


}

