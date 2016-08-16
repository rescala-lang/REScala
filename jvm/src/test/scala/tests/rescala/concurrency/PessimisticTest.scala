package tests.rescala.concurrency


import java.util.concurrent.atomic.AtomicReference
import java.util.concurrent.{ConcurrentLinkedQueue, CountDownLatch, TimeUnit}

import org.scalatest.FlatSpec
import rescala.engines.{Engine, EngineImpl}
import rescala.graph.Reactive
import rescala.parrp.{Backoff, ParRP}
import rescala.propagation.Turn
import rescala.reactives.{Signal, Signals, Var}

import scala.collection.JavaConverters._

trait PessimisticTestState {

  class PessimisticTestTurn extends ParRP(backoff = new Backoff()) {
    override def evaluate(r: Reactive[ParRP]): Unit = {
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
    def await(): Unit = {
      ready.await(1, TimeUnit.SECONDS)
      go.countDown()
    }
  }

  object Pessigen extends EngineImpl[ParRP, PessimisticTestTurn]("Pessigen", new PessimisticTestTurn) {
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

  implicit def factory: Engine[ParRP, Turn[ParRP]] = Pessigen
  def unsafeNow[T](s: Signal[T, ParRP]): T = {
    factory.plan()(s.get(_))
  }

}

class PessimisticTest extends FlatSpec {


  it should "runOnIndependentParts" in new PessimisticTestState {
      val v1 = Var(false)
      val v2 = Var(false)
      val s1 = v1.map {identity}
      val s2 = v2.map {identity}

      Pessigen.sync(s1, s2)

      val t1 = Spawn(v1.set(true))

      Thread.sleep(200)
      assert(unsafeNow(v1) === false) // turn did not finish yet

      v2.set(true)
      t1.join()

      assert(unsafeNow(s1) === true && unsafeNow(s2) === true)
      assert(Pessigen.clear() == 0)
  }

  it should "summedSignals" in new PessimisticTestState {
      val size = 100
      val sources = List.fill(size)(Var(0))
      val latch = new CountDownLatch(size)
      val mapped = sources.map(s => s.map(_ + 1))
      val sum = mapped.reduce(Signals.lift(_, _)(_ + _))
      val results = new ConcurrentLinkedQueue[Int]()
      sum.changed.+=(results.add)
      val threads = sources.map(v => Spawn {latch.countDown(); latch.await(); v.set(1)})
      threads.foreach(_.join())

      assert(results.asScala.sameElements(Range(size + 1, 2 * size + 1)))

      assert(unsafeNow(sum) === 2 * size)
      assert(Pessigen.clear() == 0)
  }

  it should "crossedDynamicDependencies" in new PessimisticTestState {
      val v1 = Var(false)
      val v2 = Var(false)
      val s11 = v1.map {identity}
      // so if s11 becomes true, this adds a dependency on v2
      val s12 = Signals.dynamic(s11) { t => if (s11(t)) v2(t) else false }
      val s21 = v2.map {identity}
      // this does as above, causing one or the other to access something which will change later
      val s22 = Signals.dynamic(s21) { t => if (s21(t)) v1(t) else false }
      var results = List[Boolean]()
      s12.changed observe { v => results ::= v }
      val c23 = s22.changed
      c23 observe { v => results ::= v }


      assert(results === Nil)

      // start both rescala.turns so they have their locks
      Pessigen.sync(s11, s21)

      // this will allow only turn 2 to continue running, causing it to wait on turn 1
      val l1 = Pessigen.syncm(s11)

      // after turn 1 continues, it will use the reactives locked by turn 2 and finish before turn 2
      val l2 = Pessigen.syncm(c23)

      val t1 = Spawn(v1.set(true))
      val t2 = Spawn(v2.set(true))

      //this is a rather poor way to test if turn 2 is already waiting, this is your chance to replace this with something smart!
      while (t2.getState != Thread.State.WAITING) {Thread.sleep(1)}
      l1.await()
      t1.join(1000)
      // still unchanged, turn 1 used the old value of v2
      assert(results === Nil)
      assert(unsafeNow(s12) === false)

      l2.await()
      t2.join(1000)

      assert(unsafeNow(s12) === true)
      assert(unsafeNow(s22) === true)
      assert(results === List(true, true))

      assert(Pessigen.clear() == 0)
  }

  object MockFacFac {
    def apply(i0: Reactive[ParRP], reg: => Unit, unreg: => Unit): Engine[ParRP, Turn[ParRP]] =
      new EngineImpl[ParRP, ParRP]("Mock Fac Fac",
        new ParRP(new Backoff()) {
          override def discover(downstream: Reactive[ParRP])(upstream: Reactive[ParRP]): Unit = {
            if (upstream eq i0) reg
            super.discover(downstream)(upstream)
          }
          override def drop(downstream: Reactive[ParRP])(upstream: Reactive[ParRP]): Unit = {
            if (upstream eq i0) unreg
            super.drop(downstream)(upstream)
          }
        })
  }

  it should "addAndRemoveDependencyInOneTurn" in new PessimisticTestState {


      val b0 = Var(false)
      val b2 = b0.map(identity).map(!_)
      val i0 = Var(11)
      var reeval = 0
      val i1_3 = Signals.dynamic(b0) { t => reeval += 1; if (b0(t) && b2(t)) i0(t) else 42 }

      var regs = 0
      var unregs = 0

      val mockFac = MockFacFac(i0, regs += 1, unregs += 1)


      assert(unsafeNow(i1_3) === 42)
      assert(reeval === 1)
      assert(regs === 0)
      assert(unregs === 0)

      // now, this should create some only in turn dynamic changes
      b0.set(true)(mockFac)

      assert(unsafeNow(i1_3) === 42)
      assert(reeval === 3)
      assert(regs === 1)
      assert(unregs === 1)

      // this does not
      b0.set(false)(mockFac)

      assert(unsafeNow(i1_3) === 42)
      assert(reeval === 4)
      assert(regs === 1)
      assert(unregs === 1)

      // this also does not, because the level of the dynamic signals stays on 3
      b0.set(true)(mockFac)

      assert(unsafeNow(i1_3) === 42)
      assert(reeval === 5)
      assert(regs === 1)
      assert(unregs === 1)
      assert(Pessigen.clear() == 0)
  }

  it should "addAndRemoveDependencyInOneTurnWhileOwnedByAnother" in new PessimisticTestState {

      val bl0 = Var(false)
      val bl1 = bl0.map(identity)
      val bl3 = bl1.map(identity).map(!_)
      val il0 = Var(11)
      val il1 = il0.map(identity)

      var reeval = 0
      // this starts on level 2. when bl0 becomes true bl1 becomes true on level 1
      // at that point both bl1 and bl3 are true which causes il1 to be added as a dependency
      // but then bl3 becomes false at level 3, causing il1 to be removed again
      // after that the level is increased and this nonesense no longer happens
      val b2b3i2 = Signals.dynamic(bl1) { t =>
        reeval += 1
        if (bl1(t)) {
          if (bl3(t)) {
            val res = il1(t)
            assert(res === 11, "did not read old value, this may happen spouriosly, probably because of the timing issue in this test")
            res
          }
          else 37
        }
        else 42
      }

      // this is here, so that we have i lock bl1.
      // we need this to be a dynamic lock to lock just this single reactive and not bl3 etc.
      val i2b2 = Signals.dynamic(il1)(t => if (il1(t) == 0) bl1(t) else false)
      val c3 = i2b2.map(identity)



      assert(unsafeNow(b2b3i2) === 42)
      assert(reeval === 1)

      // start both rescala.turns
      Pessigen.sync(bl1, il1)
      // now i has il0, il1, i2b2 locked
      // and b has bl0, bl1, bl3, b2b3i1

      // continue just turn i
      val bBar = Pessigen.syncm(bl1)
      // i unsafeNow(will) try to grab bl1, which fails
      // so i will start to wait on b

      // we start the rescala.turns …
      val t1 = Spawn {bl0.set(true)}
      val t2 = Spawn {il0.set(0)}

      // now everything will should start to happen as described above (i waiting on b)
      // we then await and release the barrier
      Thread.sleep(20)
      bBar.await()
      // which causes b to continue and evaluate b2b3i2
      // that will add and remove dependencies on il1, which we have readlocked.
      // that should NOT cause b2b3i2 to be reevaluated when i finally finishes

      t1.join()
      t2.join()

      assert(unsafeNow(b2b3i2) === 37)
      assert(reeval === 4, "did not reevaluate 4 times: init, aborted reeval, final reeval t1, final reeval t2")

      assert(Pessigen.clear() == 0)
  }

  it should "add two dynamic dependencies and remove only one" in new PessimisticTestState {

      val bl0 = Var(false)
      val bl1 = bl0.map(identity)
      val bl3 = bl1.map(identity).map(!_)
      val il0 = Var(11)
      val il1 = il0.map(identity)

      var reeval = 0
      // this starts on level 2. when bl0 becomes true bl1 becomes true on level 1
      // at that point both bl1 and bl3 are true which causes il1 and il0 to be added as a dependency
      // but then bl3 becomes false at level 3, causing il1 to be removed again (but il0 is still a dependency)
      // after that the level is increased and this nonesense no longer happens
      val b2b3i2 = Signals.dynamic(bl1) { t =>
        reeval += 1
        if (bl1(t)) {
          if (bl3(t)) {
            val res = il0(t) + il1(t)
            assert(res === 22, "did not read old value")
            res
          }
          else il0(t)
        }
        else 42
      }

      // this is here, so that we have i lock bl1.
      // we need this to be a dynamic lock to lock just this single reactive and not bl3 etc.
      val i2b2 = Signals.dynamic(il1)(t => if (il1(t) == 17) bl1(t) else false)
      val c3 = i2b2.map(identity)



      assert(unsafeNow(b2b3i2) === 42)
      assert(reeval === 1)

      // start both rescala.turns
      Pessigen.sync(bl1, il1)
      // now i has il0, il1, i2b2 locked
      // and b has bl0, bl1, bl3, b2b3i1

      // continue just turn i
      val bBar = Pessigen.syncm(bl1)
      // i unsafeNow(will) try to grab bl1, which fails
      // so i will start to wait on b

      // we start the rescala.turns …
      val t1 = Spawn {bl0.set(true)}
      val t2 = Spawn {il0.set(17)}

      // now everything will should start to happen as described above (i waiting on b)
      // we then await and release the barrier
      Thread.sleep(20)
      bBar.await()
      // which causes b to continue and evaluate b2b3i2
      // that will add and remove dependencies on il1, which we have readlocked.
      // that should still cause b2b3i2 to be reevaluated when i finally finishes (because of the dependency to il0)

      t1.join()
      t2.join()

      assert(unsafeNow(b2b3i2) === 17)
      // 4 reevaluations: initialisation; bl1 becomes true; bl3 becomes false; il0 changes from 11 to 17
      assert(reeval === 4)

      assert(Pessigen.clear() == 0)
  }

}
