package rescala.engines

import java.util.concurrent.locks.ReentrantLock

import rescala.graph.SimpleStruct
import rescala.propagation.{FactoryReference, NoLocking}

object Engines {
  type SS = SimpleStruct.type
  type NoLockEngine = Engine[SS, NoLocking[SS]]

  implicit val synchron: NoLockEngine = new EngineImpl[SS, NoLocking[SS]](SimpleStruct, new FactoryReference(SimpleStruct) with NoLocking[SS]) {
    override def plan[R](i: Reactive*)(f: NoLocking[SS] => R): R = synchronized(super.plan(i: _*)(f))
  }

  implicit val synchronFair: NoLockEngine = new EngineImpl[SS, NoLocking[SS]](SimpleStruct, new FactoryReference(SimpleStruct) with NoLocking[SS]) {
    val lock = new ReentrantLock(true)
    override def plan[R](i: Reactive*)(f: NoLocking[SS] => R): R = {
      lock.lock()
      try {
        super.plan(i: _*)(f)
      }
      finally {lock.unlock()}
    }
  }

  implicit val unmanaged: NoLockEngine = new EngineImpl[SS, NoLocking[SS]](SimpleStruct, new FactoryReference(SimpleStruct) with NoLocking[SS])

  implicit val default: NoLockEngine = synchron

  val all: List[NoLockEngine] = List(synchron, unmanaged)



}

