package tests.rescala.fullmv

import org.scalatest.FunSuite
import rescala.fullmv.{FullMVEngine, FullMVTurn, TurnPhase}
import rescala.fullmv.sgt.synchronization.SubsumableLock
import rescala.testhelper.Spawn

import scala.concurrent.{Await, TimeoutException}
import scala.concurrent.duration.Duration
import scala.util.{Failure, Try}

class LockUnionFindTest extends FunSuite {
  val engine = new FullMVEngine(Duration.Zero, "LockUnionFindTest")

  test("single lock gc works") {
    val turn = engine.newTurn()
    turn.awaitAndSwitchPhase(TurnPhase.Framing)
    val lock = turn.subsumableLock.get

    if(SubsumableLock.DEBUG) println(s"single lock gc with $turn using $lock")

    assert(lock.refCount.get === 1)
    assert(engine.getInstance(turn.guid) === Some(turn))
    assert(engine.lockHost.getInstance(lock.guid) === Some(lock))

    turn.awaitAndSwitchPhase(TurnPhase.Executing)

    assert(lock.refCount.get === 1)
    assert(engine.getInstance(turn.guid) === Some(turn))
    assert(engine.lockHost.getInstance(lock.guid) === Some(lock))

    turn.awaitAndSwitchPhase(TurnPhase.Completed)

    assert(lock.refCount.get <= 0)
    assert(engine.getInstance(turn.guid) === None)
    assert(engine.lockHost.getInstance(lock.guid) === None)
  }

  test("single subsumed gc works") {
    val turn1 = engine.newTurn()
    turn1.awaitAndSwitchPhase(TurnPhase.Framing)
    val lock1 = turn1.subsumableLock.get()

    val turn2 = engine.newTurn()
    turn2.awaitAndSwitchPhase(TurnPhase.Framing)
    val lock2 = turn2.subsumableLock.get()

    if(SubsumableLock.DEBUG) println(s"single subsumed gc with $turn1 using $lock1 and $turn2 using $lock2")

    val l1 = Await.result(turn1.lock(), Duration.Zero)
    assert(Await.result(turn2.trySubsume(l1), Duration.Zero) === true)
    l1.unlock()

    assert(lock1.refCount.get === 2) // turn2 and turn1
    assert(engine.getInstance(turn1.guid) === Some(turn1))
    assert(engine.lockHost.getInstance(lock1.guid) === Some(lock1))
    assert(lock2.refCount.get <= 0)
    assert(engine.getInstance(turn2.guid) === Some(turn2))
    assert(engine.lockHost.getInstance(lock2.guid) === None)

    turn1.awaitAndSwitchPhase(TurnPhase.Completed)

    assert(lock1.refCount.get === 1) // turn2
    assert(engine.getInstance(turn1.guid) === None)
    assert(engine.lockHost.getInstance(lock1.guid) === Some(lock1))
    assert(engine.getInstance(turn2.guid) === Some(turn2))

    turn2.awaitAndSwitchPhase(TurnPhase.Completed)

    assert(lock1.refCount.get <= 0)
    assert(engine.lockHost.getInstance(lock1.guid) === None)
    assert(engine.getInstance(turn2.guid) === None)
  }

  test("multiple subsumed gc works") {
    val maxIdx = 10
    val turns = Array.fill(maxIdx + 1) {
      val turn = engine.newTurn()
      turn.awaitAndSwitchPhase(TurnPhase.Framing)
      turn
    }
    val locks = turns.map(_.subsumableLock.get)

    turns.reduce{ (t1, t2) =>
      val l = Await.result(t2.lock(), Duration.Zero)
      assert(Await.result(t1.trySubsume(l), Duration.Zero) === true)
      l.unlock()
      t2
    }

    assert(locks(0).refCount.get <= 0) // gc'd
    assert(locks(1).refCount.get === 1) // turn 0
    for(i <- 2 until maxIdx) {
      assert(locks(i).refCount.get === 2) // lock(i-1), turn(i-1)
    }
    assert(locks(maxIdx).refCount.get === 3) // lock(count-1), turn(count-1), turn(count)

    turns(1).awaitAndSwitchPhase(TurnPhase.Completed)
    turns(2).awaitAndSwitchPhase(TurnPhase.Completed)
    turns(4).awaitAndSwitchPhase(TurnPhase.Completed)

    assert(locks(0).refCount.get <= 0) // gc'd
    assert(locks(1).refCount.get === 1) // turn 0
    assert(locks(2).refCount.get === 1) // lock 1
    assert(locks(3).refCount.get === 1) // lock 2
    assert(locks(4).refCount.get === 2) // turn 3, lock 3
    assert(locks(5).refCount.get === 1) // lock 4
    for(i <- 6 until maxIdx) {
      assert(locks(i).refCount.get === 2) // lock(i-1), turn(i-1)
    }
    assert(locks(maxIdx).refCount.get === 3) // lock(count-1), turn(count-1), turn(count)

    Await.result(turns(0).lock(), Duration.Zero).unlock()

    assert(locks(0).refCount.get <= 0) // gc'd
    assert(locks(1).refCount.get <= 0) // gc'd
    assert(locks(2).refCount.get <= 0) // gc'd
    assert(locks(3).refCount.get <= 0) // gc'd
    assert(locks(4).refCount.get === 1) // turn 3
    assert(locks(5).refCount.get <= 0) // gc'd
    for(i <- 6 until maxIdx) {
      assert(locks(i).refCount.get === 1) // turn(i-1)
    }
    assert(locks(maxIdx).refCount.get === maxIdx - 6 + 4) // lock(4), lock(6) to lock(maxIdx - 1), turn(0), turn(count-1), turn(count)
  }

  test("lock works") {
    // we can lock
    val a = engine.newTurn()
    val res = Spawn{ Await.result(a.lock(), Duration.Zero) }.join(101)

    // lock is exclusive and blocks
    val blockedB = Spawn{ Await.result(a.lock(), Duration.Zero) }
    intercept[TimeoutException] { blockedB.join(103) }

    // unlock unblocks
    res.unlock()
    assert(blockedB.join(104) === res)
  }

  test("union works") {
    val a = engine.newTurn()
    val b = engine.newTurn()

    val res = Spawn{ Await.result(b.lock(), Duration.Zero) }.join(101)

    assert(Await.result(a.trySubsume(res), Duration.Zero) === true)

    assert(Await.result(a.getLockedRoot, Duration.Zero) === Some(res.guid))
    assert(Await.result(b.getLockedRoot, Duration.Zero) === Some(res.guid))

    res.unlock()

    val res2 = Spawn{ Await.result(a.lock(), Duration.Zero) }.join(102)
    val blockedA = Spawn{ Await.result(a.lock(), Duration.Zero).unlock() }
    intercept[TimeoutException] { blockedA.join(103) }
    val blockedB = Spawn{ Await.result(b.lock(), Duration.Zero).unlock() }
    intercept[TimeoutException] { blockedB.join(104) }

    res2.unlock()

    blockedA.join(105)
    blockedB.join(106)
  }

  test("subsume correctly wakes all threads") {
    val a, b = engine.newTurn()
    val res = Await.result(a.lock(), Duration.Zero)
    assert(Await.result(b.trySubsume(res), Duration.Zero) === true)

    var counter = 0
    def spawnIncrementUnderLockThread(lockable: FullMVTurn) = {
      Spawn {
        val r = Await.result(lockable.lock(), Duration.Zero)
        val c = counter
        counter += 1
        r.unlock()
        c -> r.guid
      }
    }

    val queued = List.fill(5){ spawnIncrementUnderLockThread(a) } ++ List.fill(5){ spawnIncrementUnderLockThread(b) }

    val timeout = System.currentTimeMillis() + 50
    val timeouts = queued.map { thread => Try { thread.join(timeout - System.currentTimeMillis()) } }
    assert(!timeouts.exists{
      case Failure(_: TimeoutException) => false
      case _ => true
    }, s"All threads should have timed out, but some did not.")

    res.unlock()

    assert(queued.map(_.join(50)).toSet === (0 until 10).map(_ -> res.guid).toSet)
  }
}
