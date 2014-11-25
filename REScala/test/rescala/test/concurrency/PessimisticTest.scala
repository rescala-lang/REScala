package rescala.test.concurrency


import java.util.concurrent.{ConcurrentLinkedQueue, CountDownLatch}

import org.junit.Test
import org.scalatest.junit.AssertionsForJUnit
import rescala.Var
import rescala.propagation.Reactive
import rescala.propagation.turns.creation.TurnFactory
import rescala.propagation.turns.creation.TurnFactory.Impl
import rescala.propagation.turns.instances.Pessimistic
import rescala.signals.Signals

import scala.collection.JavaConverters._

class PessimisticTestTurn extends Pessimistic {
  override def evaluate(r: Reactive): Unit = {
    if (Pessigen.syncSet(r)) {
      Pessigen.latch.countDown()
      Pessigen.latch.await()
    }
    super.evaluate(r)
  }
}
object Pessigen extends TurnFactory.Impl(new PessimisticTestTurn) {
  @volatile var latch: CountDownLatch = new CountDownLatch(0)
  @volatile var syncSet: Set[Reactive] = Set()

  def sync(reactives: Reactive*): Unit = {
    latch = new CountDownLatch(reactives.size)
    syncSet = reactives.toSet
  }

}

class PessimisticTest extends AssertionsForJUnit {

  implicit def factory: TurnFactory = Pessigen

  @Test def runOnIndependentParts(): Unit = {
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
  }

  @Test def summedSignals(): Unit = {
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
  }

  @Test def crossedDynamicDependencies(): Unit = {
    val v1 = Var(false)
    val v2 = Var(false)
    val s11 = v1.map { identity }
    val s12 = Signals.dynamic(s11) { t => if (s11(t)) v2(t) else false }
    val s21 = v2.map { identity }
    val s22 = Signals.dynamic(s21) { t => if (s21(t)) v1(t) else false }
    var results = List[Boolean]()
    s12.changed.+= { v => results ::= v }
    s22.changed.+= { v => results ::= v }


    assert(results === Nil)

    Pessigen.sync(s11, s21)

    val t = Spawn(v1.set(true))
    v2.set(true)
    t.join()

    assert(results === List(true))
  }


  object MockFacFac {
    def apply(i0: Reactive, reg: => Unit, unreg: => Unit): TurnFactory = new Impl(
      new Pessimistic {
        override def register(downstream: Reactive)(upstream: Reactive): Unit = {
          if (upstream eq i0) reg
          super.register(downstream)(upstream)
        }
        override def unregister(downstream: Reactive)(upstream: Reactive): Unit = {
          if (upstream eq i0) unreg
          super.unregister(downstream)(upstream)
        }
      })
  }

  @Test def addAndRemoveDependencyInOneTurn(): Unit = {


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
  }

}
