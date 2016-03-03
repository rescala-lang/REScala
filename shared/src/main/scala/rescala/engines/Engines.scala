package rescala.engines

import java.util.concurrent.locks.ReentrantLock

import rescala.graph._
import rescala.propagation.LevelBasedPropagation

object Engines {
  type SS = SimpleStruct.type
  type SimpleEngine = Engine[SS, LevelBasedPropagation[SS]]

  private class SimpleNoLock extends LevelBasedPropagation[SS] {
    override val bufferFactory = SimpleStruct
    override def releasePhase(): Unit = ()
    override def pulses[P](budP: SS#SporeP[P, Reactive[SS]]): Buffer[Pulse[P]] = budP.pulses.asInstanceOf[Buffer[Pulse[P]]]
    override def incoming[R](bud: LevelSporeImpl[_, R]): Set[R] = bud.incoming(this)
    override def updateIncoming[R](bud: LevelSporeImpl[_, R], newDependencies: Set[R]): Unit = bud.updateIncoming(newDependencies)(this)
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

