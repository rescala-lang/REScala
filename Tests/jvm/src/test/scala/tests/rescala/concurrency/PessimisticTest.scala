package tests.rescala.concurrency

import java.util.concurrent.{ConcurrentLinkedQueue, CountDownLatch}

import org.scalatest.FlatSpec
import rescala.engine.{Engine, Turn}
import rescala.graph.Reactive
import rescala.parrp.{Backoff, ParRP}
import rescala.reactives.{Signals, Var}
import rescala.testhelper.PessimisticTestState
import rescala.twoversion.EngineImpl

import scala.collection.JavaConverters._



class PessimisticTest extends FlatSpec {


  it should "run On Independent Parts" in new PessimisticTestState {
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

  it should "summed Signals" in new PessimisticTestState {
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

  it should "crossed Dynamic Dependencies" in new PessimisticTestState {
      val v1 = Var(0)
      val v2 = Var(1)
      val s11 = v1.map {identity}
      // so if s11 becomes true, this adds a dependency on v2
      val s12 = engine.dynamic(s11) {t =>  if (t.depend(s11) != 0) t.depend(v2) else 2 }
      val s21 = v2.map {identity}
      // this does as above, causing one or the other to access something which will change later
      val s22 = engine.dynamic(s21) {t => if (t.depend(s21) != 1) t.depend(v1) else 3 }
      var results1 = List[Int]()
      s12.changed observe { v => results1 ::= v }
      val c23 = s22.changed
      var results2 = List[Int]()
      c23 observe { v => results2 ::= v }

      assert(results1 === Nil)
      assert(results2 === Nil)

      // start both rescala.turns so they have their locks
      Pessigen.sync(s11, s21)

      // this will allow only turn 2 to continue running, causing it to wait on turn 1
      val l1 = Pessigen.syncm(s11)

      // after turn 1 continues, it will use the reactives locked by turn 2 and finish before turn 2
      val l2 = Pessigen.syncm(c23)

      val t1 = Spawn(v1.set(4))
      val t2 = Spawn(v2.set(5))

      //this is a rather poor way to test if turn 2 is already waiting, this is your chance to replace this with something smart!
      while (t2.getState != Thread.State.WAITING) {Thread.sleep(1)}
      l1.await()
      t1.join(1000)
      // turn 1 used the old value of v2
      assert(results1 === List(1))
      assert(results2 === Nil)
      assert(unsafeNow(s12) === 1)

      l2.await()
      t2.join(1000)

      assert(unsafeNow(s12) === 5)
      assert(unsafeNow(s22) === 4)
      assert(results1 === List(5, 1))
      assert(results2 === List(4))

      assert(Pessigen.clear() == 0)
  }

  object MockFacFac {
    def apply(i0: Reactive[ParRP], reg: => Unit, unreg: => Unit): Engine[ParRP, Turn[ParRP]] =
      new EngineImpl[ParRP, ParRP]("Mock Fac Fac",
        new ParRP(new Backoff(), None) {
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

  it should "(not?) Add And Remove Dependency In One Turn" in new PessimisticTestState {
      // this behavior is not necessary for correctness; adding and removing the edge (i.e. regs and unregs +=1)
      // would be equally correct. It is implemented purely to discover accidental behavior changes, but should
      // have its exepected results changed upon intentional behavior changes!
      val b0 = Var(false)
      val b2 = b0.map(identity).map(!_)
      val i0 = Var(11)
      var reeval = 0
      val i1_3 = engine.dynamic(b0) { t => reeval += 1 : @unchecked; if (t.depend(b0) && t.depend(b2)) t.depend(i0) else 42 }

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
      assert(regs === 0)
      assert(unregs === 0)

      // this does not
      b0.set(false)(mockFac)

      assert(unsafeNow(i1_3) === 42)
      assert(reeval === 4)
      assert(regs === 0)
      assert(unregs === 0)

      // this also does not, because the level of the dynamic signals stays on 3
      b0.set(true)(mockFac)

      assert(unsafeNow(i1_3) === 42)
      assert(reeval === 5)
      assert(regs === 0)
      assert(unregs === 0)
      assert(Pessigen.clear() == 0)
  }

  it should "not retrofit a reevaluation for t2, after a dependency might have been added and removed again inside a single t1 While Owned By t2" in new PessimisticTestState {

      val bl0 = Var(false)
      val bl1 = bl0.map(identity)
      val bl3 = bl1.map(identity).map(!_)
      val il0 = Var(11)
      val il1 = il0.map(identity)

      var reeval = List.empty[Any]
      // this starts on level 2. when bl0 becomes true bl1 becomes true on level 1
      // at that point both bl1 and bl3 are true which causes il1 to be added as a dependency
      // but then bl3 becomes false at level 3, causing il1 to be removed again
      // after that the level is increased and this nonesense no longer happens
      val b2b3i2 = engine.dynamic(bl1) { t =>
        reeval ::= t.turn
        if (t.depend(bl1)) {
          if (t.depend(bl3)) {
            val res = t.depend(il1)
            assert(res === 11, "did not read old value, this may happen spouriosly, probably because of the timing issue in this test")
            res
          }
          else 37
        }
        else 42
      }

      // this is here, so that we have i lock bl1.
      // we need this to be a dynamic lock to lock just this single reactive and not bl3 etc.
      val i2b2 = engine.dynamic(il1)(t => if (t.depend(il1) == 0) t.depend(bl1) else false)
      val c3 = i2b2.map(identity)

      // bl0 -> bl1 -> (bl2) -> bl3
      //           >--------------`--> b2b3i2
      // il0 -> il1  `-> i2b2 -> c3

      assert(unsafeNow(b2b3i2) === 42)
      assert(reeval.size === 1)

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
      assert(reeval.size == 3, ": b2b3i2 did not reevaluate 3 times (init, aborted reeval t1, final reeval t1)")

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
      val b2b3i2 = engine.dynamic(bl1) { t =>
        reeval += 1 : @unchecked
        if (t.depend(bl1)) {
          if (t.depend(bl3)) {
            val res = t.depend(il0) + t.depend(il1)
            assert(res === 22, "did not read old value")
            res
          }
          else t.depend(il0)
        }
        else 42
      }

      // this is here, so that we have i lock bl1.
      // we need this to be a dynamic lock to lock just this single reactive and not bl3 etc.
      val i2b2 = engine.dynamic(il1)( t =>if (t.depend(il1) == 17) t.depend(bl1) else false)
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

    // 4 reevaluations: initialisation; bl1 becomes true; bl3 becomes false; il0 changes from 11 to 17
      assert((reeval, unsafeNow(b2b3i2)) ===( (4, 17) ))

      assert(Pessigen.clear() == 0)
  }

}
