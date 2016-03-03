package rescala.engines

import java.util.concurrent.locks.ReentrantLock

import rescala.graph._
import rescala.propagation.LevelBasedPropagation

object Engines {
  class SimpleNoLock extends LevelBasedPropagation[SimpleStruct[SimpleNoLock]] {
    def bud[P, R](initialValue: Pulse[P], transient: Boolean, initialIncoming: Set[R]): SimpleStruct[SimpleNoLock]#SporeP[P, R] =
      new LevelSporeImpl[P, R](initialValue, transient, initialIncoming)
    override def releasePhase(): Unit = ()
  }

  type SimpleEngine = Engine[SimpleStruct[SimpleNoLock], LevelBasedPropagation[SimpleStruct[SimpleNoLock]]]


  implicit val synchron: SimpleEngine = new EngineImpl[SimpleStruct[SimpleNoLock], SimpleNoLock](new SimpleNoLock()) {
    override def plan[R](i: Reactive*)(f: SimpleNoLock => R): R = synchronized(super.plan(i: _*)(f))
  }

  implicit val synchronFair: SimpleEngine = new EngineImpl[SimpleStruct[SimpleNoLock], SimpleNoLock](new SimpleNoLock()) {
    val lock = new ReentrantLock(true)
    override def plan[R](i: Reactive*)(f: SimpleNoLock => R): R = {
      lock.lock()
      try {
        super.plan(i: _*)(f)
      }
      finally {lock.unlock()}
    }
  }

  implicit val unmanaged: SimpleEngine = new EngineImpl[SimpleStruct[SimpleNoLock], SimpleNoLock](new SimpleNoLock())

  implicit val default: SimpleEngine = synchron

  val all: List[SimpleEngine] = List(synchron, unmanaged)


}

