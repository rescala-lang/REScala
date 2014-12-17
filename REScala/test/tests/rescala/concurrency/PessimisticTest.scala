package tests.rescala.concurrency


import java.util.concurrent.atomic.AtomicReference
import java.util.concurrent.{ConcurrentLinkedQueue, CountDownLatch}

import org.junit.Test
import org.scalatest.junit.AssertionsForJUnit
import rescala.graph.Reactive
import rescala.synchronization.{Pessimistic, EngineReference, Prelock}
import rescala.turns.{Turn, Engine, Engines}
import rescala.{Signals, Var}

import scala.collection.JavaConverters._

class PessimisticTestTurn extends Pessimistic {
  override def evaluate(r: Reactive): Unit = {
    while (Pessigen.syncStack.get() match {
      case stack@(set, bar) :: tail if set(r) =>
        bar.ready.countDown()
        bar.go.await()
        Pessigen.syncStack.compareAndSet(stack, tail)
        true
      case _ => false
    }) {}
    super.evaluate(r)
  }
}

case class Barrier(ready: CountDownLatch, go: CountDownLatch) {
  def await() = {
    ready.await()
    go.countDown()
  }
}

object Pessigen extends Engines.Impl(new PessimisticTestTurn) {
  val syncStack: AtomicReference[List[(Set[Reactive], Barrier)]] = new AtomicReference(Nil)

  def clear(): Int = syncStack.getAndSet(Nil).size

  def sync(reactives: Reactive*): Unit = {
    val bar = syncm(reactives: _*)
    Spawn(bar.await())
  }

  def syncm(reactives: Reactive*): Barrier = {
    val ready = new CountDownLatch(reactives.size)
    val go = new CountDownLatch(1)
    val syncSet = reactives.toSet
    val bar = Barrier(ready, go)
    syncStack.set(syncStack.get() :+ ((syncSet, bar)))
    bar
  }

}

class PessimisticTest extends AssertionsForJUnit {

  implicit def factory: Engine[Turn] = Pessigen

  @Test def runOnIndependentParts(): Unit = synchronized {
    val v1 = Var(false)
    val v2 = Var(false)
    val s1 = v1.map { identity }
    val s2 = v2.map { identity }

    Pessigen.sync(s1, s2)

    val t1 = Spawn(v1.set(true))

    Thread.sleep(200)
    assert(v1.now === false) // turn did not finish yet

    v2.set(true)
    t1.join()

    assert(s1.now === true && s2.now === true)
    assert(Pessigen.clear() == 0)
  }

  @Test def summedSignals(): Unit = synchronized {
    val size = 100
    val sources = List.fill(size)(Var(0))
    val latch = new CountDownLatch(1)
    val mapped = sources.map(s => s.map(_ + 1))
    val sum = mapped.reduce(Signals.lift(_, _)(_ + _))
    val results = new ConcurrentLinkedQueue[Int]()
    sum.changed.+=(results.add)
    val threads = sources.map(v => Spawn { latch.await(); v.set(1) })
    latch.countDown()
    threads.foreach(_.join())

    assert(results.asScala.sameElements(Range(size + 1, 2 * size + 1)))

    assert(sum.now === 2 * size)
    assert(Pessigen.clear() == 0)
  }

  @Test def crossedDynamicDependencies(): Unit = synchronized {
    val v1 = Var(false)
    val v2 = Var(false)
    val s11 = v1.map { identity }
    // so if s11 becomes true, this adds a dependency on v2
    val s12 = Signals.dynamic(s11) { t => if (s11(t)) v2(t) else false }
    val s21 = v2.map { identity }
    // this does as above, causing one or the other to access something which will change later
    val s22 = Signals.dynamic(s21) { t => if (s21(t)) v1(t) else false }
    var results = List[Boolean]()
    s12.changed observe { v => results ::= v }
    val c23 = s22.changed
    c23 observe { v => results ::= v }


    assert(results === Nil)

    // start both turns so they have their locks
    Pessigen.sync(s11, s21)

    // this will allow only turn 2 to continue running, causing it to wait on turn 1
    val l1 = Pessigen.syncm(s11)

    // after turn 1 continues, it will use the reactives locked by turn 2 and finish before turn 2
    val l2 = Pessigen.syncm(c23)

    val t1 = Spawn(v1.set(true))
    val t2 = Spawn(v2.set(true))

    //this is a rather poor way to test if turn 2 is already waiting, this is your chance to replace this with something smart!
    while (t2.getState != Thread.State.WAITING) { Thread.sleep(1) }
    l1.await()
    t1.join()
    // still unchanged, turn 1 used the old value of v2
    assert(results === Nil)
    assert(s12.now === false)

    l2.await()
    t2.join()

    assert(s12.now === true)
    assert(s22.now === true)
    assert(results === List(true, true))

    assert(Pessigen.clear() == 0)
  }


  object MockFacFac {
    def apply(i0: Reactive, reg: => Unit, unreg: => Unit): Engine[Turn] = {
      lazy val engine: Engine[Prelock] = new Engines.Impl[Prelock](
        new EngineReference(engine) with Prelock {
        override def register(downstream: Reactive)(upstream: Reactive): Unit = {
          if (upstream eq i0) reg
          super.register(downstream)(upstream)
        }
        override def unregister(downstream: Reactive)(upstream: Reactive): Unit = {
          if (upstream eq i0) unreg
          super.unregister(downstream)(upstream)
        }
      })
      engine
    }
  }

  @Test def addAndRemoveDependencyInOneTurn(): Unit = synchronized {


    val b0 = Var(false)
    val b2 = b0.map(identity).map(!_)
    val i0 = Var(11)
    var reeval = 0
    val i1_3 = Signals.dynamic(b0) { t => reeval += 1; if (b0(t) && b2(t)) i0(t) else 42 }

    var regs = 0
    var unregs = 0

    val mockFac = MockFacFac(i0, regs += 1, unregs += 1)


    assert(i1_3.now === 42)
    assert(reeval === 1)
    assert(regs === 0)
    assert(unregs === 0)

    // now, this should create some only in turn dynamic changes
    b0.set(true)(mockFac)

    assert(i1_3.now === 42)
    assert(reeval === 3)
    assert(regs === 1)
    assert(unregs === 1)

    // this does not
    b0.set(false)(mockFac)

    assert(i1_3.now === 42)
    assert(reeval === 4)
    assert(regs === 1)
    assert(unregs === 1)

    // this also does not, because the level of the dynamic signals stays on 3
    b0.set(true)(mockFac)

    assert(i1_3.now === 42)
    assert(reeval === 5)
    assert(regs === 1)
    assert(unregs === 1)
    assert(Pessigen.clear() == 0)
  }


  @Test def addAndRemoveDependencyInOneTurnWhileOwnedByAnother(): Unit = for (_ <- Range(0, 100)) synchronized {

    val b0 = Var(false)
    val b1 = b0.map(identity)
    val b2 = b1.map(identity).map(!_)
    val i0 = Var(11)
    val i1 = i0.map(identity)

    var reeval = 0
    // this starts on level 2. when b0 becomes true b1 becomes true on level 1
    // at that point both b1 and b2 are true which causes i1 to be added as a dependency
    // but then b2 becomes false at level 2, causing i1 to be removed again
    // after that the level is increased and this nonesense no longer happens
    val b2b3i2 = Signals.dynamic(b1) { t => reeval += 1; if (b1(t) && b2(t)) i1(t) else 42 }

    // this is here, so that we have another turn, that locks b1.
    // we need this to be a dynamic lock to lock just this single reactive and not b2 etc.
    val i2b2 = Signals.dynamic(i1)(t => if (i1(t) == 0) b1(t) else false)
    val c3 = i2b2.map(identity)



    assert(b2b3i2.now === 42)
    assert(reeval === 1)

    // start both turns
    Pessigen.sync(b1, i1)
    // now i has i0, i1, i2b2 locked
    // and b has b0, b1, b2, b2b3i1

    // continue just turn i
    val bBar = Pessigen.syncm(b1)
    // i will now try to grab b1, which fails
    // so i will start to wait on b

    // we start the turns …
    val t1 = Spawn { b0.set(true) }
    val t2 = Spawn { i0.set(0) }

    // now everything will should start to happen as described above (i waiting on b)
    // we then await and release the barrier
    bBar.await()
    // which causes b to continue and evaluate b2b3i2
    // that will add and remove dependencies on i1, which we have readlocked.
    // that should NOT cause b2b3i2 to be reevaluated when i finally finishes

    t1.join()
    t2.join()

    assert(b2b3i2.now === 42)
    assert(reeval === 3)

    assert(Pessigen.clear() == 0)
  }

}
