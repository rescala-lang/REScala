package tests.rescala.concurrency

import rescala.core.{Initializer, ScopeSearch}
import rescala.core.infiltration.JVMInfiltrator
import rescala.parrp.Backoff
import rescala.scheduler
import rescala.scheduler.Schedulers
import tests.rescala.testtools.{RETests, *}

import java.util.concurrent.{CountDownLatch, TimeUnit}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.*
import scala.concurrent.{Await, Future}

class PessimisticTest extends RETests {
  engines(Schedulers.parrp)("SynchronizedReevaluation should synchronize reevaluations") { engine =>
    val sra = new SynchronizedReevaluationApi(engine)
    val rta = new ReevaluationBundle[sra.api.type](sra.api)
    import rta._
    import sra._
    import sra.api._

    val v1          = Var(false)
    val v2          = Var(false)
    val (sync1, s1) = SynchronizedReevaluation(v1)
    val (sync2, s2) = SynchronizedReevaluation(v2)
    val trackS1     = new ReevaluationTracker(s1)
    val reached1    = notifyOnceReached(sync1)
    val autoSync    = autoSyncNextReevaluation(sync1, sync2)

    val t1 = Spawn { v1.set(true) }

    assert(reached1.await(1000, TimeUnit.MILLISECONDS))
    trackS1.assertClear(false)      // turn did not finish yet
    assert(autoSync.getCount === 1) // but has reduced latch to only second turn missing

    v2.set(true)
    t1.await(1000)
    assert(autoSync.getCount == 0)

    assert(s1.readValueOnce === true)
    trackS1.assertClear(true)
    assert(s2.readValueOnce === true)
  }

  engines(scheduler.Schedulers.parrp)(
    "Pessimistic Engines should safely execute concurrently admitted updates to summed signals"
  ) { engine =>
    val rta = new ReevaluationBundle[engine.type](engine)
    import engine._
    import rta._

    val size    = 10
    val sources = List.fill(size)(Var(0))

    val sum        = sources.map(_.map(identity)).reduce(Signal.lift(_, _)(_ + _))
    val sumTracker = new ReevaluationTracker(sum)

    val latch = new CountDownLatch(size)
    val threads = sources.map(v =>
      Spawn {
        latch.countDown()
        latch.await()
        v.set(1)
      }
    )

    val timeout = System.currentTimeMillis() + 1000
    threads.foreach(_.await(math.max(0, timeout - System.currentTimeMillis())))
    assert(latch.getCount == 0)

    sumTracker.assert((0 to size).reverse: _*)
    assert(sum.readValueOnce === size)
  }

  "Pessimistic Engines should correctly execute crossed dynamic discoveries" in {
    for (_ <- 1 to 1000) {
      val interface = scheduler.Schedulers.parrp
      val sra       = new SynchronizedReevaluationApi[interface.type](interface)
      val rta       = new ReevaluationBundle[sra.api.type](sra.api)
      import interface._
      import rta._
      import sra._

      val initA = "init a"
      val initB = "init b"

      val staticA = "static a"
      val staticB = "static b"

      val vA = Var(initA)("var A")
      val vB = Var(initB)("var B")

      val (syncInA, syncA) = SynchronizedReevaluation(vA)("sync A")
      val (syncInB, syncB) = SynchronizedReevaluation(vB)("sync B")

      // so if syncA becomes true, this adds a dependency on vB
      val crossA =
        interface.Signal.dynamic(syncA) { t => if (t.depend(syncA) != initA) t.depend(syncB) else staticA }("crossA")

      // this does as above, causing one or the other to access something which will change later
      val crossB =
        interface.Signal.dynamic(syncB) { t => if (t.depend(syncB) != initB) t.depend(syncA) else staticB }("crossB")

      val resultsA = new ReevaluationTracker(crossA)("resultsA")
      val resultsB = new ReevaluationTracker(crossB)("resultsB")

      resultsA.assertClear(staticA)
      resultsB.assertClear(staticB)

      // force both turns to start executing before either may perform its dynamic discoveries
      val syncBothLatch = autoSyncNextReevaluation(syncInA, syncInB)

      // no evaluations happend yet
      resultsA.assertClear()
      resultsB.assertClear()

      val finalA = "final a"
      val finalB = "final b"
      val t1     = Spawn(vA.set(finalA), Some("setting A thread"))
      val t2     = Spawn(vB.set(finalB), Some("setting B thread"))

      assert(syncBothLatch.await(10000, TimeUnit.MILLISECONDS))

      t1.await(10000)
      t2.await(10000)

      // turn 1 used the old value of vB and the retrofitted reevaluation for turn 2 executed and used the new value
      assert((resultsA.results == List(finalB, initB) && resultsB.results == List(finalA)) ||
        (resultsA.results == List(finalB) && resultsB.results == List(finalA, initA)))
    }
  }

  "ParRP should (not?) Add And Remove Dependency In One Turn" in {
    import rescala.scheduler.Schedulers.parrp._
    import JVMInfiltrator.unsafeNow

    // this behavior is not necessary for correctness; adding and removing the edge (i.e. regs and unregs +=1)
    // would be equally correct. It is implemented purely to discover accidental behavior changes, but should
    // have its exepected results changed upon intentional behavior changes!
    val b0     = Var(false)
    val b2     = b0.map(identity).map(!_) // dirty hacks to get il_3 to reevaluate first on levelbased engines
    val i0     = Var(11)
    var reeval = 0
    val i1_3 = Signal.dynamic(b0) { t =>
      reeval += 1; if (t.depend(b0) && t.depend(b2)) t.depend(i0) else 42
    }

    var regs   = 0
    var unregs = 0

    val mockFac = new TwoVersionScheduler[ParRPTransaction] {
      override def schedulerName: String = "Reg/Unreg counting ParRP"
      override protected def makeTransaction(priorTx: Option[ParRPTransaction]): ParRPTransaction =
        new ParRPTransaction(new Backoff(), None) {
          override def discover(source: ReSource, sink: Derived): Unit = {
            if (source eq i0) regs += 1
            super.discover(source, sink)
          }
          override def drop(source: ReSource, sink: Derived): Unit = {
            if (source eq i0) unregs += 1
            super.drop(source, sink)
          }
        }
    }

    assert(unsafeNow(i1_3) === 42)
    assert(reeval === 1)
    assert(regs === 0)
    assert(unregs === 0)

    // now, this should create some only in turn dynamic changes
    b0.set(true)(mockFac, ScopeSearch.fromSchedulerImplicit(mockFac))

    assert(unsafeNow(i1_3) === 42)
    assert(reeval === 3)
    assert(regs === 0)
    assert(unregs === 0)

    // this does not
    b0.set(false)(mockFac, ScopeSearch.fromSchedulerImplicit(mockFac))

    assert(unsafeNow(i1_3) === 42)
    assert(reeval === 4)
    assert(regs === 0)
    assert(unregs === 0)

    // this also does not, because the level of the dynamic signals stays on 3
    b0.set(true)(mockFac, ScopeSearch.fromSchedulerImplicit(mockFac))

    assert(unsafeNow(i1_3) === 42)
    assert(reeval === 5)
    assert(regs === 0)
    assert(unregs === 0)
  }

  engines(scheduler.Schedulers.parrp)(
    "Pessimistic Engines should not retrofit a reevaluation for t2, after a dependency might have been added and removed again inside a single t1 While Owned By t2"
  ) { engine =>
    import engine._
    val sra = new SynchronizedReevaluationApi[engine.type](engine)
    import sra._
    val rta = new ReevaluationBundle[engine.type](engine)
    import rta._
    val saea = new SetAndExtractTransactionHandle[engine.type](engine)
    import saea._

    val bl0                  = Var(false)
    val (syncB1, bl1)        = SynchronizedReevaluation(bl0)
    var bl3: Signal[Boolean] = null // dirty hacks to get b2b3i2 to reevaluate first on non-levelbased engines
    val il0                  = Var(11)
    val (syncI1, il1)        = SynchronizedReevaluation(il0)

    var reeval = List.empty[Initializer[BundleState]]
    // this starts on level 2. when bl0 becomes true bl1 becomes true on level 1
    // at that point both bl1 and bl3 are true which causes il1 to be added as a dependency
    // but then bl3 becomes false at level 3, causing il1 to be removed again
    // after that the level is increased and this nonesense no longer happens
    val b2b3i2 = engine.Signal.dynamic(bl1) { t =>
      reeval ::= t.tx.initializer
      if (t.depend(bl1)) {
        if (t.depend(bl3)) {
          val res = t.depend(il1)
          assert(
            res === 11,
            "did not read old value, this may happen spouriosly, probably because of the timing issue in this test"
          )
          res
        } else 37
      } else 42
    }
    val results = new ReevaluationTracker(b2b3i2)

    bl3 = bl1.map(identity).map(!_) // dirty hacks to get b2b3i2 to reevaluate first on levelbased engines

    // this is here, so that we have i lock bl1.
    // we need this to be a dynamic lock to lock just this single reactive and not bl3 etc.
    val i2b2     = engine.Signal.dynamic(il1)(t => if (t.depend(il1) == 0) t.depend(bl1) else false)
    val results2 = new ReevaluationTracker(i2b2)

    // bl0 -> bl1 -> (bl2) -> bl3
    //           >--------------`--> b2b3i2
    // il0 -> il1  `-> i2b2

    results.assertClear(42)
    assert(reeval.size === 1)
    reeval = List() // drop creation turn
    results2.assertClear(false)

    // require both turns to start executing before either may perform dynamic discoveries
    val latch = autoSyncNextReevaluation(syncB1, syncI1)
    // i has il0, il1, i2b2 locked
    // b has bl0, bl1, bl3, b2b3i1

    // further force the "b" turn to wait for manual approval, so that the second turn will execute its discovery first
    val latch1 = manuallySyncNextReevaluation(syncB1)

    // we start the rescala.turns …
    // i will try to grab bl1, which is locked by b, so i will start to wait on b
    val t1 = Future { SetAndExtractTransactionHandle(bl0, true) }
    val t2 = Future { SetAndExtractTransactionHandle(il0, 0) }

    assert(latch.await(1000, TimeUnit.MILLISECONDS))
    assert(latch1.getCount === 1) // b should wait for manual approval only
    results2.assertClear() // i should not have propagated over the dynamic discovery, despite not being blocked manually

    latch1.countDown()
    // which causes b to continue and evaluate b2b3i2
    // that will add and remove dependencies on il1, which we have readlocked.
    // that should NOT cause b2b3i2 to be reevaluated when i finally finishes
    val turn1 = Await.result(t1, 1000.milliseconds)
    val turn2 = Await.result(t2, 1000.milliseconds)

    results.assertClear(37)
    results2.assertClear(true)
    assert(Set(List(turn1, turn1), List(turn1)).contains(reeval), " -- for reference, turn2 was " + turn2)
  }

  engines(scheduler.Schedulers.parrp)(
    "pessimistic engines should add two dynamic dependencies and remove only one",
    List(IgnoreOnGithubCiBecause("test failes sometimes …"))
  ) { engine =>
    import engine._
    val sra = new SynchronizedReevaluationApi[engine.type](engine)
    import sra._
    val rta = new ReevaluationBundle[engine.type](engine)
    import rta._
    val saea = new SetAndExtractTransactionHandle[engine.type](engine)
    import saea._
    val bl0                  = Var(false)
    val (syncB1, bl1)        = SynchronizedReevaluation(bl0)
    var bl3: Signal[Boolean] = null // dirty hacks to ensure that b2b3i2 is reevaluated first
    val il0                  = Var(11)
    val (syncI1, il1)        = SynchronizedReevaluation(il0)

    var reeval = List.empty[Initializer[BundleState]]
    // this starts on level 2. when bl0 becomes true bl1 becomes true on level 1
    // at that point both bl1 and bl3 are true which causes il1 and il0 to be added as a dependency
    // but then bl3 becomes false at level 3, causing il1 to be removed again (but il0 is still a dependency)
    // after that the level is increased and this nonesense no longer happens
    val b2b3i2 = engine.Signal.dynamic(bl1) { t =>
      reeval ::= t.tx.initializer
      if (t.depend(bl1)) {
        if (t.depend(bl3)) {
          val res = t.depend(il0) + t.depend(il1)
          assert(res === 22, "did not read old value")
          res
        } else t.depend(il0)
      } else 42
    }
    val results = new ReevaluationTracker(b2b3i2)
    bl3 = bl1.map(identity).map(!_) // dirty hacks to get b2b3i2 to reevaluate first on levelbased engines

    // this is here, so that we have i lock bl1.
    // we need this to be a dynamic lock to lock just this single reactive and not bl3 etc.
    val i2b2     = engine.Signal.dynamic(il1)(t => if (t.depend(il1) == 17) t.depend(bl1) else false)
    val results2 = new ReevaluationTracker(i2b2)

    // bl0 -> bl1 -> (bl2) -> bl3
    //           >--------------`--> b2b3i2
    // il0 -> il1  `-> i2b2 -> c3

    results.assertClear(42)
    assert(reeval.size === 1)
    reeval = List() // drop creation turn
    results2.assertClear(false)

    // require both turns to start executing before either may perform dynamic discoveries
    val latch = autoSyncNextReevaluation(syncB1, syncI1)
    // i has il0, il1, i2b2 locked
    // b has bl0, bl1, bl3, b2b3i1

    // further force the "b" turn to wait for manual approval, so that the second turn will execute its discovery first
    val latch1 = manuallySyncNextReevaluation(syncB1)

    // we start the rescala.turns …
    // i will try to grab bl1, which is locked by b, so i will start to wait on b
    val t1 = Future { SetAndExtractTransactionHandle(bl0, true) }
    val t2 = Future { SetAndExtractTransactionHandle(il0, 17) }

    assert(latch.await(1000, TimeUnit.MILLISECONDS))
    assert(latch1.getCount === 1) // b should wait for manual approval only
    results2.assertClear() // i should not have propagated over the dynamic discovery, despite not being blocked manually

    latch1.countDown()
    // which causes b to continue and evaluate b2b3i2
    // that will add a dependencay on i10 and add and remove dependencies on il1, which we have both readlocked.
    // that SHUOLD cause b2b3i2 to be reevaluated when i finally finishes (because the dependency to il0 remains)

    val turn1 = Await.result(t1, 1000.milliseconds)
    val turn2 = Await.result(t2, 1000.milliseconds)

    results2.assertClear(true)
    results.assertClear(17, 11)
    assert(Set(List(turn2, turn1, turn1), List(turn2, turn1)).contains(reeval))
  }

}
