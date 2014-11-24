package rescala.test.concurrency


import java.util.concurrent.{ConcurrentLinkedQueue, CountDownLatch, TimeUnit}

import org.junit.{Ignore, Test}
import org.scalatest.junit.AssertionsForJUnit
import rescala.Var
import rescala.signals.Signals

import scala.collection.JavaConverters._


class PessimisticTest extends AssertionsForJUnit {

  import rescala.Implicits.pessimistic

  @Test def runOnIndependentParts(): Unit = {
    val v1 = Var(false)
    val v2 = Var(false)
    val latch = new CountDownLatch(2)
    val s1 = v1.map{ v => if (v) {latch.countDown(); latch.await(1, TimeUnit.SECONDS)} else false}
    val s2 = v2.map{ v => if (v) {latch.countDown(); latch.await(1, TimeUnit.SECONDS)} else false}

    val t = Spawn(v1.set(true))
    v2.set(true)
    t.join()

    assert(s1.now === true && s2.now === true)
    assert(latch.getCount === 0)
  }

  @Test def summedSignals(): Unit = {
    val size = 100
    val sources = List.fill(size)(Var(0))
    val latch = new CountDownLatch(1)
    val mapped = sources.map(s => s.map(_ + 1))
    val sum = mapped.reduce(Signals.lift(_, _)(_ + _))
    val threads = sources.map(v => Spawn{latch.await(); v.set(1)})
    val results = new ConcurrentLinkedQueue[Int]()
    sum.changed.+=(results.add)
    latch.countDown()
    threads.foreach(_.join())

    assert(results.asScala.sameElements(Range(size + 1, 2 * size + 1)))

    assert(sum.now === 2 * size)
  }

  @Ignore def crossedDynamicDependencies(): Unit = {
    val v1 = Var(false)
    val v2 = Var(false)
    val latch = new CountDownLatch(2)
    val s11 = v1.map{ v => if (v) {latch.countDown(); latch.await(1, TimeUnit.SECONDS)} else false}
    val s12 = Signals.dynamic(s11){ t => if (s11(t)) v2(t) else false }
    val s21 = v2.map{ v => if (v) {latch.countDown(); latch.await(1, TimeUnit.SECONDS)} else false}
    val s22 = Signals.dynamic(s21){ t => if (s21(t)) v1(t) else false }
    var results = List[Boolean]()
    s12.changed.+={v => println(v); results ::= v}
    s22.changed.+={v => println(v); results ::= v}


    assert(results === Nil)

    val t = Spawn(v1.set(true))
    v2.set(true)
    t.join()

    assert(results === List(true))
    assert(latch.getCount === 0)
  }

}
