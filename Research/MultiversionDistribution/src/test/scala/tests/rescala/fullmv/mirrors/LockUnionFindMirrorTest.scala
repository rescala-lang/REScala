package tests.rescala.fullmv.mirrors

import org.scalatest.FunSuite
import rescala.fullmv.FullMVEngine
import rescala.fullmv.mirrors.localcloning.FullMVTurnLocalClone
import rescala.fullmv.sgt.synchronization.{Blocked, Locked, Successful}

import scala.concurrent.duration.Duration
import scala.concurrent.Await

class LockUnionFindMirrorTest extends FunSuite {
  test("double remote lock/block/unlock works") {
    val host1a = new FullMVEngine(Duration.Zero, "host1")
    val host1b = new FullMVEngine(Duration.Zero, "host2")
    val hostX = new FullMVEngine(Duration.Zero, "host3")

    val turn1 = host1a.newTurn()
    val lock1 = turn1.subsumableLock.get
    turn1.beginExecuting()

    val turn1onB = FullMVTurnLocalClone(turn1, host1b)
    val turn1onX = FullMVTurnLocalClone(turn1onB, hostX)

    assert(lock1.refCount.get === 1) // turn1
    assert(host1b.lockHost.getInstance(lock1.guid).isEmpty)
    assert(hostX.lockHost.getInstance(lock1.guid).isEmpty)

    val lock1onX = Await.result(turn1onX.tryLock(), Duration.Zero).asInstanceOf[Locked].lock
    assert(lock1onX === lock1)
    assert((lock1onX ne lock1) === true)

    assert(lock1.refCount.get === 2) // turn1, lock1onB
    val lock1onB = host1b.lockHost.getInstance(lock1.guid).get
    assert(lock1onB.refCount.get === 1) // lock1onX
    assert(hostX.lockHost.getInstance(lock1.guid).get eq lock1onX)
    assert(lock1onX.refCount.get === 1) // thread

    assert(Await.result(turn1.tryLock(), Duration.Zero) === Blocked)

    assert(lock1.refCount.get === 2) // turn1, lock1onB
    assert(host1b.lockHost.getInstance(lock1.guid).get eq lock1onB)
    assert(lock1onB.refCount.get === 1) // lock1onX
    assert(hostX.lockHost.getInstance(lock1.guid).get eq lock1onX)
    assert(lock1onX.refCount.get === 1) // thread

    assert(Await.result(turn1onB.tryLock(), Duration.Zero) === Blocked)

    assert(lock1.refCount.get === 2) // turn1, lock1onB
    assert(host1b.lockHost.getInstance(lock1.guid).get eq lock1onB)
    assert(lock1onB.refCount.get === 1) // lock1onX
    assert(hostX.lockHost.getInstance(lock1.guid).get eq lock1onX)
    assert(lock1onX.refCount.get === 1) // thread

    assert(Await.result(turn1onX.tryLock(), Duration.Zero) === Blocked)

    assert(lock1.refCount.get === 2) // turn1, lock1onB
    assert(host1b.lockHost.getInstance(lock1.guid).get eq lock1onB)
    assert(lock1onB.refCount.get === 1) // lock1onX
    assert(hostX.lockHost.getInstance(lock1.guid).get eq lock1onX)
    assert(lock1onX.refCount.get === 1) // thread

    lock1onX.asyncUnlock()

    assert(lock1.refCount.get === 1) // turn1
    assert(lock1onB.refCount.get <= 0)
    assert(host1b.lockHost.getInstance(lock1.guid).isEmpty)
    assert(lock1onX.refCount.get <= 0)
    assert(hostX.lockHost.getInstance(lock1.guid).isEmpty)

    val lock1onB2 = Await.result(turn1onB.tryLock(), Duration.Zero).asInstanceOf[Locked].lock
    assert(lock1onB === lock1onB2)
    assert(lock1onB ne lock1onB2)

    assert(lock1.refCount.get === 2) // turn1, lock1onB2
    assert(host1b.lockHost.getInstance(lock1.guid).get eq lock1onB2)
    assert(lock1onB2.refCount.get === 1) // thread
    assert(lock1onB.refCount.get <= 0)
    assert(hostX.lockHost.getInstance(lock1.guid).isEmpty)
    assert(lock1onX.refCount.get <= 0)

    lock1onB2.asyncUnlock()

    assert(lock1.refCount.get === 1) // turn1
    assert(host1b.lockHost.getInstance(lock1.guid).isEmpty)
    assert(lock1onB2.refCount.get <= 0)
    assert(lock1onB.refCount.get <= 0)
    assert(hostX.lockHost.getInstance(lock1.guid).isEmpty)
    assert(lock1onX.refCount.get <= 0)

    turn1.completeExecuting()
  }

  test("blocked remote subsume leaves reference counts intact") {
    val host1a = new FullMVEngine(Duration.Zero, "host1")
    val host1b = new FullMVEngine(Duration.Zero, "host2")
    val hostX = new FullMVEngine(Duration.Zero, "host3")

    val turn1 = host1a.newTurn()
    val lock1 = turn1.subsumableLock.get
    turn1.beginExecuting()

    val turn1onB = FullMVTurnLocalClone(turn1, host1b)
    val turn1onX = FullMVTurnLocalClone(turn1onB, hostX)

    assert(lock1.refCount.get === 1) // turn1
    assert(host1b.lockHost.getInstance(lock1.guid).isEmpty)
    assert(hostX.lockHost.getInstance(lock1.guid).isEmpty)

    val lock1onX = Await.result(turn1onX.tryLock(), Duration.Zero).asInstanceOf[Locked].lock
    assert(lock1onX === lock1)
    assert((lock1onX ne lock1) === true)

    assert(lock1.refCount.get === 2) // turn1, lock1onB
    val lock1onB = host1b.lockHost.getInstance(lock1.guid).get
    assert(lock1onB.refCount.get === 1) // lock1onX
    assert(hostX.lockHost.getInstance(lock1.guid).get eq lock1onX)
    assert(lock1onX.refCount.get === 1) // thread

    val turn2 = hostX.newTurn()
    turn2.beginExecuting()
    val l2 = Await.result(turn2.tryLock(), Duration.Zero).asInstanceOf[Locked].lock
    assert(l2.refCount.get === 2) // turn2, thread

    assert(Await.result(turn1onX.trySubsume(l2), Duration.Zero) === Blocked)

    assert(lock1.refCount.get === 2) // turn1, lock1onB
    assert(host1b.lockHost.getInstance(lock1.guid).get eq lock1onB)
    assert(lock1onB.refCount.get === 1) // lock1onX
    assert(hostX.lockHost.getInstance(lock1.guid).get eq lock1onX)
    assert(lock1onX.refCount.get === 1) // thread
    assert(l2.refCount.get === 2) // turn2, thread
  }

  test("single remote subsume and gc works") {
    val host1 = new FullMVEngine(Duration.Zero, "host1")

    val turn1 = host1.newTurn()
    turn1.beginExecuting()
    val lock1 = turn1.subsumableLock.get

    val host2 = new FullMVEngine(Duration.Zero, "host2")
    val turn1on2 = FullMVTurnLocalClone(turn1, host2)
    val lock1on2 = Await.result(turn1on2.tryLock(), Duration.Zero).asInstanceOf[Locked].lock

    assert(lock1on2 === lock1)
    assert((lock1on2 ne lock1) === true)

    assert(lock1.refCount.get === 2) // turn1, lock1on2
    assert(lock1on2.refCount.get === 1) // thread

    val turn2 = host2.newTurn()
    turn2.beginExecuting()
    val lock2 = turn2.subsumableLock.get

    assert(Await.result(turn2.trySubsume(lock1on2), Duration.Zero) === Successful)

    assert(lock1.refCount.get === 2) // turn1, lock1on2
    assert(lock1on2.refCount.get === 2) // thread, turn2
    assert(lock2.refCount.get <= 0)

    lock1on2.asyncUnlock()
    assert(lock1.refCount.get === 2) // turn1, lock1on2
    assert(lock1on2.refCount.get === 1) // turn2
    assert(lock2.refCount.get <= 0)

    turn1.completeExecuting()
    assert(lock1.refCount.get === 1) // lock1on2
    assert(lock1on2.refCount.get === 1) // turn2
    assert(lock2.refCount.get <= 0)

    turn2.completeExecuting()
    assert(lock1.refCount.get <= 0)
    assert(lock1on2.refCount.get <= 0)
    assert(lock2.refCount.get <= 0)
  }

  test("quintuple remote single subsume and gc works") {
    val host1a = new FullMVEngine(Duration.Zero, "host1")
    val host1b = new FullMVEngine(Duration.Zero, "host2")
    val host2a = new FullMVEngine(Duration.Zero, "host3")
    val host2b = new FullMVEngine(Duration.Zero, "host3")
    val hostX = new FullMVEngine(Duration.Zero, "host3")

    val turn1 = host1a.newTurn()
    val lock1 = turn1.subsumableLock.get
    turn1.beginExecuting()

    val turn2 = host2a.newTurn()
    val lock2 = turn2.subsumableLock.get
    turn2.beginExecuting()

    val turn1onB = FullMVTurnLocalClone(turn1, host1b)
    val turn1onX = FullMVTurnLocalClone(turn1onB, hostX)
    val turn2onB = FullMVTurnLocalClone(turn2, host2b)
    val turn2onX = FullMVTurnLocalClone(turn2onB, hostX)

    assert(lock1.refCount.get === 1) // turn1
    assert(host1a.lockHost.getInstance(lock1.guid).get eq lock1)
    assert(host1b.lockHost.getInstance(lock1.guid).isEmpty)
    assert(host2a.lockHost.getInstance(lock1.guid).isEmpty)
    assert(host2b.lockHost.getInstance(lock1.guid).isEmpty)
    assert(hostX.lockHost.getInstance(lock1.guid).isEmpty)

    assert(lock2.refCount.get === 1) // turn2
    assert(host1a.lockHost.getInstance(lock2.guid).isEmpty)
    assert(host1b.lockHost.getInstance(lock2.guid).isEmpty)
    assert(host2a.lockHost.getInstance(lock2.guid).get eq lock2)
    assert(host2b.lockHost.getInstance(lock2.guid).isEmpty)
    assert(hostX.lockHost.getInstance(lock2.guid).isEmpty)

    val lock1onX = Await.result(turn1onX.tryLock(), Duration.Zero).asInstanceOf[Locked].lock
    assert(lock1onX === lock1)
    assert((lock1onX ne lock1) === true)

    assert(lock1.refCount.get === 2) // turn1, lock1onB
    assert(host1a.lockHost.getInstance(lock1.guid).get eq lock1)
    val lock1onB = host1b.lockHost.getInstance(lock1.guid).get
    assert(lock1onB.refCount.get === 1) // lock1onX
    assert(host2a.lockHost.getInstance(lock1.guid).isEmpty)
    assert(host2b.lockHost.getInstance(lock1.guid).isEmpty)
    assert(lock1onX.refCount.get === 1) // thread
    assert(hostX.lockHost.getInstance(lock1.guid).get eq lock1onX)

    assert(lock2.refCount.get === 1) // turn2
    assert(host1a.lockHost.getInstance(lock2.guid).isEmpty)
    assert(host1b.lockHost.getInstance(lock2.guid).isEmpty)
    assert(host2a.lockHost.getInstance(lock2.guid).get eq lock2)
    assert(host2b.lockHost.getInstance(lock2.guid).isEmpty)
    assert(hostX.lockHost.getInstance(lock2.guid).isEmpty)

    assert(Await.result(turn2onX.trySubsume(lock1onX), Duration.Zero) === Successful)

    assert(lock1.refCount.get === 2) // turn1, lock1onB
    assert(host1a.lockHost.getInstance(lock1.guid).get eq lock1)
    assert(lock1onB.refCount.get === 1) // lock1onX
    assert(host1b.lockHost.getInstance(lock1.guid).get eq lock1onB)
    val lock1on2a = host2a.lockHost.getInstance(lock1.guid).get
    assert(lock1on2a.refCount.get === 1) // turn2
    val lock1on2b = host2b.lockHost.getInstance(lock1.guid).get
    assert(lock1on2b.refCount.get === 1) // lock1on2a
    assert(lock1onX.refCount.get === 2) // thread, lock1on2b
    assert(hostX.lockHost.getInstance(lock1.guid).get eq lock1onX)

    assert(lock2.refCount.get <= 0)
    assert(host1a.lockHost.getInstance(lock2.guid).isEmpty)
    assert(host1b.lockHost.getInstance(lock2.guid).isEmpty)
    assert(host2a.lockHost.getInstance(lock2.guid).isEmpty)
    assert(host2b.lockHost.getInstance(lock2.guid).isEmpty)
    assert(hostX.lockHost.getInstance(lock2.guid).isEmpty)

    lock1onX.asyncUnlock()

    assert(lock1.refCount.get === 2) // turn1, lock1onB
    assert(host1a.lockHost.getInstance(lock1.guid).get eq lock1)
    assert(lock1onB.refCount.get === 1) // lock1onX
    assert(host1b.lockHost.getInstance(lock1.guid).get eq lock1onB)
    assert(lock1on2a.refCount.get === 1) // turn2
    assert(host2a.lockHost.getInstance(lock1.guid).get eq lock1on2a)
    assert(lock1on2b.refCount.get === 1) // lock1on2a
    assert(host2b.lockHost.getInstance(lock1.guid).get eq lock1on2b)
    assert(lock1onX.refCount.get === 1) // lock1on2b
    assert(hostX.lockHost.getInstance(lock1.guid).get eq lock1onX)

    turn1.completeExecuting()

    assert(lock1.refCount.get === 1) // lock1onB
    assert(host1a.lockHost.getInstance(lock1.guid).get eq lock1)
    assert(lock1onB.refCount.get === 1) // lock1onX
    assert(host1b.lockHost.getInstance(lock1.guid).get eq lock1onB)
    assert(lock1on2a.refCount.get === 1) // turn2
    assert(host2a.lockHost.getInstance(lock1.guid).get eq lock1on2a)
    assert(lock1on2b.refCount.get === 1) // lock1on2a
    assert(host2b.lockHost.getInstance(lock1.guid).get eq lock1on2b)
    assert(lock1onX.refCount.get === 1) // lock1on2b
    assert(hostX.lockHost.getInstance(lock1.guid).get eq lock1onX)

    turn2.completeExecuting()

    assert(host1a.lockHost.getInstance(lock1.guid).isEmpty)
    assert(lock1.refCount.get <= 0)
    assert(host1b.lockHost.getInstance(lock1.guid).isEmpty)
    assert(lock1onB.refCount.get <= 0)
    assert(host2a.lockHost.getInstance(lock1.guid).isEmpty)
    assert(lock1on2a.refCount.get <= 0)
    assert(host2b.lockHost.getInstance(lock1.guid).isEmpty)
    assert(lock1on2b.refCount.get <= 0)
    assert(hostX.lockHost.getInstance(lock1.guid).isEmpty)
    assert(lock1onX.refCount.get <= 0)
  }
}
