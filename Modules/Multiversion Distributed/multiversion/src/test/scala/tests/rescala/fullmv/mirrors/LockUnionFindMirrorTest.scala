package tests.rescala.fullmv.mirrors

import org.scalatest.funsuite.AnyFunSuite
import rescala.fullmv.DistributedFullMVApi.{FullMVEngine, FullMVTurnLocalClone, SubsumableLock}
import rescala.fullmv.sgt.synchronization.{Blocked, Locked, Successful}

import scala.concurrent.Await
import scala.concurrent.duration.Duration

class LockUnionFindMirrorTest extends AnyFunSuite {
  test("double remote direct lock/block/unlock works") {
    val host1a = new FullMVEngine(Duration.Zero, "host1a")
    val host1b = new FullMVEngine(Duration.Zero, "host1b")
    val hostX  = new FullMVEngine(Duration.Zero, "hostX")

    val turn1 = host1a.newTurn()
    val lock1 = turn1.subsumableLock.get
    turn1.beginExecuting()

    val turn1onB = FullMVTurnLocalClone(turn1, host1b)
    val turn1onX = FullMVTurnLocalClone(turn1onB, hostX)

    assert(lock1.refCount.get === 1) // turn1
    assert(host1b.lockHost.getInstance(lock1.guid).isEmpty)
    assert(hostX.lockHost.getInstance(lock1.guid).isEmpty)

    val lock1onX = Await.result(turn1onX.tryLock(), Duration.Zero).asInstanceOf[Locked].lock
    assert(lock1onX.remotelyEquals(lock1))
    assert((lock1onX !== lock1) === true)

    assert(lock1.refCount.get === 2) // turn1, lock1onB
    val lock1onB = host1b.lockHost.getInstance(lock1.guid).get
    assert(lock1onB.refCount.get === 1) // lock1onX
    assert(hostX.lockHost.getInstance(lock1.guid).get === lock1onX)
    assert(lock1onX.refCount.get === 1) // thread

    assert(Await.result(turn1.tryLock(), Duration.Zero) === Blocked)

    assert(lock1.refCount.get === 2) // turn1, lock1onB
    assert(host1b.lockHost.getInstance(lock1.guid).get === lock1onB)
    assert(lock1onB.refCount.get === 1) // lock1onX
    assert(hostX.lockHost.getInstance(lock1.guid).get === lock1onX)
    assert(lock1onX.refCount.get === 1) // thread

    assert(Await.result(turn1onB.tryLock(), Duration.Zero) === Blocked)

    assert(lock1.refCount.get === 2) // turn1, lock1onB
    assert(host1b.lockHost.getInstance(lock1.guid).get === lock1onB)
    assert(lock1onB.refCount.get === 1) // lock1onX
    assert(hostX.lockHost.getInstance(lock1.guid).get === lock1onX)
    assert(lock1onX.refCount.get === 1) // thread

    assert(Await.result(turn1onX.tryLock(), Duration.Zero) === Blocked)

    assert(lock1.refCount.get === 2) // turn1, lock1onB
    assert(host1b.lockHost.getInstance(lock1.guid).get === lock1onB)
    assert(lock1onB.refCount.get === 1) // lock1onX
    assert(hostX.lockHost.getInstance(lock1.guid).get === lock1onX)
    assert(lock1onX.refCount.get === 1) // thread

    lock1onX.asyncUnlock()

    assert(lock1.refCount.get === 1) // turn1
    assert(lock1onB.refCount.get <= 0)
    assert(host1b.lockHost.getInstance(lock1.guid).isEmpty)
    assert(lock1onX.refCount.get <= 0)
    assert(hostX.lockHost.getInstance(lock1.guid).isEmpty)

    val lock1onB2 = Await.result(turn1onB.tryLock(), Duration.Zero).asInstanceOf[Locked].lock
    assert(lock1onB.remotelyEquals(lock1onB2))
    assert(lock1onB !== lock1onB2)

    assert(lock1.refCount.get === 2) // turn1, lock1onB2
    assert(host1b.lockHost.getInstance(lock1.guid).get === lock1onB2)
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

  test("blocked remote direct subsume leaves reference counts intact") {
    val host1a = new FullMVEngine(Duration.Zero, "host1a")
    val host1b = new FullMVEngine(Duration.Zero, "host1b")
    val hostX  = new FullMVEngine(Duration.Zero, "hostX")

    val turn1 = host1a.newTurn()
    val lock1 = turn1.subsumableLock.get
    turn1.beginExecuting()

    val turn1onB = FullMVTurnLocalClone(turn1, host1b)
    val turn1onX = FullMVTurnLocalClone(turn1onB, hostX)

    assert(lock1.refCount.get === 1) // turn1
    assert(host1b.lockHost.getInstance(lock1.guid).isEmpty)
    assert(hostX.lockHost.getInstance(lock1.guid).isEmpty)

    val lock1onX = Await.result(turn1onX.tryLock(), Duration.Zero).asInstanceOf[Locked].lock
    assert(lock1onX.remotelyEquals(lock1))
    assert((lock1onX !== lock1) === true)

    assert(lock1.refCount.get === 2) // turn1, lock1onB
    val lock1onB = host1b.lockHost.getInstance(lock1.guid).get
    assert(lock1onB.refCount.get === 1) // lock1onX
    assert(hostX.lockHost.getInstance(lock1.guid).get === lock1onX)
    assert(lock1onX.refCount.get === 1) // thread

    val turn2 = hostX.newTurn()
    turn2.beginExecuting()
    val l2 = Await.result(turn2.tryLock(), Duration.Zero).asInstanceOf[Locked].lock
    assert(l2.refCount.get === 2) // turn2, thread

    assert(Await.result(turn1onX.trySubsume(l2), Duration.Zero) === Blocked)

    assert(lock1.refCount.get === 2) // turn1, lock1onB
    assert(host1b.lockHost.getInstance(lock1.guid).get === lock1onB)
    assert(lock1onB.refCount.get === 1) // lock1onX
    assert(hostX.lockHost.getInstance(lock1.guid).get === lock1onX)
    assert(lock1onX.refCount.get === 1) // thread
    assert(l2.refCount.get === 2)       // turn2, thread
  }

  test("single remote direct subsume and gc works") {
    val host1 = new FullMVEngine(Duration.Zero, "host1")

    val turn1 = host1.newTurn()
    turn1.beginExecuting()
    val lock1 = turn1.subsumableLock.get

    val host2    = new FullMVEngine(Duration.Zero, "host2")
    val turn1on2 = FullMVTurnLocalClone(turn1, host2)
    val lock1on2 = Await.result(turn1on2.tryLock(), Duration.Zero).asInstanceOf[Locked].lock

    assert(lock1on2.remotelyEquals(lock1))
    assert(lock1on2 !== lock1)

    assert(lock1.refCount.get === 2)    // turn1, lock1on2
    assert(lock1on2.refCount.get === 1) // thread

    val turn2 = host2.newTurn()
    turn2.beginExecuting()
    val lock2 = turn2.subsumableLock.get

    assert(Await.result(turn2.trySubsume(lock1on2), Duration.Zero) === Successful)

    assert(lock1.refCount.get === 2)    // turn1, lock1on2
    assert(lock1on2.refCount.get === 2) // thread, turn2
    assert(lock2.refCount.get <= 0)

    lock1on2.asyncUnlock()
    assert(lock1.refCount.get === 2)    // turn1, lock1on2
    assert(lock1on2.refCount.get === 1) // turn2
    assert(lock2.refCount.get <= 0)

    turn1.completeExecuting()
    assert(lock1.refCount.get === 1)    // lock1on2
    assert(lock1on2.refCount.get === 1) // turn2
    assert(lock2.refCount.get <= 0)

    turn2.completeExecuting()
    assert(lock1.refCount.get <= 0)
    assert(lock1on2.refCount.get <= 0)
    assert(lock2.refCount.get <= 0)
  }

  test("quintuple remote direct single subsume and gc works") {
    val host1a = new FullMVEngine(Duration.Zero, "host1a")
    val host1b = new FullMVEngine(Duration.Zero, "host1b")
    val host2a = new FullMVEngine(Duration.Zero, "host2a")
    val host2b = new FullMVEngine(Duration.Zero, "host2b")
    val hostX  = new FullMVEngine(Duration.Zero, "hostX")

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
    assert(host1a.lockHost.getInstance(lock1.guid).get === lock1)
    assert(host1b.lockHost.getInstance(lock1.guid).isEmpty)
    assert(host2a.lockHost.getInstance(lock1.guid).isEmpty)
    assert(host2b.lockHost.getInstance(lock1.guid).isEmpty)
    assert(hostX.lockHost.getInstance(lock1.guid).isEmpty)

    assert(lock2.refCount.get === 1) // turn2
    assert(host1a.lockHost.getInstance(lock2.guid).isEmpty)
    assert(host1b.lockHost.getInstance(lock2.guid).isEmpty)
    assert(host2a.lockHost.getInstance(lock2.guid).get === lock2)
    assert(host2b.lockHost.getInstance(lock2.guid).isEmpty)
    assert(hostX.lockHost.getInstance(lock2.guid).isEmpty)

    val lock1onX = Await.result(turn1onX.tryLock(), Duration.Zero).asInstanceOf[Locked].lock
    assert(lock1onX.remotelyEquals(lock1))
    assert((lock1onX !== lock1) === true)

    assert(lock1.refCount.get === 2) // turn1, lock1onB
    assert(host1a.lockHost.getInstance(lock1.guid).get === lock1)
    val lock1onB = host1b.lockHost.getInstance(lock1.guid).get
    assert(lock1onB.refCount.get === 1) // lock1onX
    assert(host2a.lockHost.getInstance(lock1.guid).isEmpty)
    assert(host2b.lockHost.getInstance(lock1.guid).isEmpty)
    assert(lock1onX.refCount.get === 1) // thread
    assert(hostX.lockHost.getInstance(lock1.guid).get === lock1onX)

    assert(lock2.refCount.get === 1) // turn2
    assert(host1a.lockHost.getInstance(lock2.guid).isEmpty)
    assert(host1b.lockHost.getInstance(lock2.guid).isEmpty)
    assert(host2a.lockHost.getInstance(lock2.guid).get === lock2)
    assert(host2b.lockHost.getInstance(lock2.guid).isEmpty)
    assert(hostX.lockHost.getInstance(lock2.guid).isEmpty)

    assert(Await.result(turn2onX.trySubsume(lock1onX), Duration.Zero) === Successful)

    assert(lock1.refCount.get === 2) // turn1, lock1onB
    assert(host1a.lockHost.getInstance(lock1.guid).get === lock1)
    assert(lock1onB.refCount.get === 1) // lock1onX
    assert(host1b.lockHost.getInstance(lock1.guid).get === lock1onB)
    val lock1on2a = host2a.lockHost.getInstance(lock1.guid).get
    assert(lock1on2a.refCount.get === 1) // turn2
    val lock1on2b = host2b.lockHost.getInstance(lock1.guid).get
    assert(lock1on2b.refCount.get === 1) // lock1on2a
    assert(lock1onX.refCount.get === 2)  // thread, lock1on2b
    assert(hostX.lockHost.getInstance(lock1.guid).get === lock1onX)

    assert(lock2.refCount.get <= 0)
    assert(host1a.lockHost.getInstance(lock2.guid).isEmpty)
    assert(host1b.lockHost.getInstance(lock2.guid).isEmpty)
    assert(host2a.lockHost.getInstance(lock2.guid).isEmpty)
    assert(host2b.lockHost.getInstance(lock2.guid).isEmpty)
    assert(hostX.lockHost.getInstance(lock2.guid).isEmpty)

    lock1onX.asyncUnlock()

    assert(lock1.refCount.get === 2) // turn1, lock1onB
    assert(host1a.lockHost.getInstance(lock1.guid).get === lock1)
    assert(lock1onB.refCount.get === 1) // lock1onX
    assert(host1b.lockHost.getInstance(lock1.guid).get === lock1onB)
    assert(lock1on2a.refCount.get === 1) // turn2
    assert(host2a.lockHost.getInstance(lock1.guid).get === lock1on2a)
    assert(lock1on2b.refCount.get === 1) // lock1on2a
    assert(host2b.lockHost.getInstance(lock1.guid).get === lock1on2b)
    assert(lock1onX.refCount.get === 1) // lock1on2b
    assert(hostX.lockHost.getInstance(lock1.guid).get === lock1onX)

    turn1.completeExecuting()

    assert(lock1.refCount.get === 1) // lock1onB
    assert(host1a.lockHost.getInstance(lock1.guid).get === lock1)
    assert(lock1onB.refCount.get === 1) // lock1onX
    assert(host1b.lockHost.getInstance(lock1.guid).get === lock1onB)
    assert(lock1on2a.refCount.get === 1) // turn2
    assert(host2a.lockHost.getInstance(lock1.guid).get === lock1on2a)
    assert(lock1on2b.refCount.get === 1) // lock1on2a
    assert(host2b.lockHost.getInstance(lock1.guid).get === lock1on2b)
    assert(lock1onX.refCount.get === 1) // lock1on2b
    assert(hostX.lockHost.getInstance(lock1.guid).get === lock1onX)

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

  test("direct reentrant tryLock works") {
    val host1a = new FullMVEngine(Duration.Zero, "host1a")
    val host1b = new FullMVEngine(Duration.Zero, "host1b")
    val hostX  = new FullMVEngine(Duration.Zero, "hostX")
    val turn1  = host1a.newTurn()
    turn1.beginExecuting()
    val lock1 = turn1.subsumableLock.get()

    if (SubsumableLock.DEBUG) println(s"reentrant trylock with $turn1 under $lock1")
    val turn1onB = FullMVTurnLocalClone(turn1, host1b)
    val turn1onX = FullMVTurnLocalClone(turn1onB, hostX)

    assert(lock1.refCount.get === 1) // turn1
    assert(host1a.lockHost.getInstance(lock1.guid).get === lock1)
    assert(host1b.lockHost.getInstance(lock1.guid).isEmpty)
    assert(hostX.lockHost.getInstance(lock1.guid).isEmpty)

    val l1 = Await.result(turn1onX.tryLock(), Duration.Zero).asInstanceOf[Locked].lock

    assert(lock1.refCount.get === 2) // Turn1, lock1onB
    assert(host1a.lockHost.getInstance(lock1.guid).get === lock1)
    val lock1onB = host1b.lockHost.getInstance(lock1.guid).get
    assert(lock1onB.refCount.get === 1) // lock1onX
    val lock1onX = hostX.lockHost.getInstance(lock1.guid).get
    assert(lock1onX.refCount.get === 1) // thread

    assert(Await.result(turn1onX.trySubsume(l1), Duration.Zero) === Successful)

    assert(lock1.refCount.get === 2) // Turn1, lock1onB
    assert(host1a.lockHost.getInstance(lock1.guid).get === lock1)
    assert(lock1onB.refCount.get === 1) // lock1onX
    assert(host1b.lockHost.getInstance(lock1.guid).get === lock1onB)
    assert(lock1onX.refCount.get === 1) // thread
    assert(hostX.lockHost.getInstance(lock1.guid).get === lock1onX)

    l1.asyncUnlock()

    assert(lock1.refCount.get === 1) // turn1
    assert(host1a.lockHost.getInstance(lock1.guid).get === lock1)
    assert(host1b.lockHost.getInstance(lock1.guid).isEmpty)
    assert(hostX.lockHost.getInstance(lock1.guid).isEmpty)

    turn1.completeExecuting()

    assert(lock1.refCount.get <= 1)
    assert(host1a.lockHost.getInstance(lock1.guid).isEmpty)
    assert(host1b.lockHost.getInstance(lock1.guid).isEmpty)
    assert(hostX.lockHost.getInstance(lock1.guid).isEmpty)
  }

  test("remote indirect and direct blocked-self trySubsume works") {
    val host1 = new FullMVEngine(Duration.Zero, "host1")
    val host2 = new FullMVEngine(Duration.Zero, "host2")
    val turn1 = host1.newTurn()
    turn1.beginExecuting()
    val lock1 = turn1.subsumableLock.get()

    val turn2 = host2.newTurn()
    turn2.beginExecuting()
    val lock2 = turn2.subsumableLock.get()

    val turn3 = host2.newTurn()
    turn3.beginExecuting()
    val lock3 = turn3.subsumableLock.get()

    val l3       = Await.result(turn3.tryLock(), Duration.Zero).asInstanceOf[Locked].lock
    val turn1on2 = FullMVTurnLocalClone(turn1, host2)
    val lock1on2 = Await.result(turn1on2.tryLock(), Duration.Zero).asInstanceOf[Locked].lock
    assert(Await.result(turn2.trySubsume(lock1on2), Duration.Zero) === Successful)

    assert(lock1on2.remotelyEquals(lock1))
    assert(lock1on2 !== lock1)
    assert(lock1.refCount.get === 2)    // turn1, lock1on2
    assert(lock1on2.refCount.get === 2) // turn2, thread
    assert(lock2.refCount.get <= 0)
    assert(lock3.refCount.get === 2) // turn3, thread
    assert(host1.lockHost.getInstance(lock3.guid).isEmpty)

    assert(Await.result(turn2.trySubsume(l3), Duration.Zero) === Blocked)

    assert(lock1.refCount.get === 2)    // turn1, lock1on2
    assert(lock1on2.refCount.get === 2) // turn2, thread
    assert(lock3.refCount.get === 2)    // turn3, thread
    assert(host1.lockHost.getInstance(lock3.guid).isEmpty)

    lock1on2.asyncUnlock()

    assert(lock1.refCount.get === 2)    // turn1, lock1on2
    assert(lock1on2.refCount.get === 1) // turn2
    assert(host2.lockHost.getInstance(lock1.guid).get === lock1on2)
    assert(lock3.refCount.get === 2) // turn3, thread
    assert(host1.lockHost.getInstance(lock3.guid).isEmpty)

    assert(Await.result(turn2.trySubsume(l3), Duration.Zero) === Successful)

    assert(lock1.refCount.get === 1)   // turn1
    assert(lock1on2.refCount.get <= 0) // turn2
    assert(host2.lockHost.getInstance(lock1.guid).isEmpty)
    assert(lock3.refCount.get === 4) // turn2, turn3, thread, lock3on1
    val lock3on1 = host1.lockHost.getInstance(lock3.guid).get
    assert(lock3on1.refCount.get === 1) // lock1
  }

  test("remote direct blocked-other trySubsume works") {
    val host1 = new FullMVEngine(Duration.Zero, "host1")
    val host2 = new FullMVEngine(Duration.Zero, "host2")
    val host3 = new FullMVEngine(Duration.Zero, "host3")

    val turn1 = host1.newTurn()
    turn1.beginExecuting()
    val lock1 = turn1.subsumableLock.get()

    val turn2 = host2.newTurn()
    turn2.beginExecuting()
    val lock2 = turn2.subsumableLock.get()

    val turn3 = host3.newTurn()
    turn3.beginExecuting()
    val lock3 = turn3.subsumableLock.get()

    val turn1on2 = FullMVTurnLocalClone(turn1, host2)
    val lock1on2 = Await.result(turn1on2.tryLock(), Duration.Zero).asInstanceOf[Locked].lock
    assert(Await.result(turn2.trySubsume(lock1on2), Duration.Zero) === Successful)
    lock1on2.asyncUnlock()

    val turn3on1 = FullMVTurnLocalClone(turn3, host1)
    val lock3on1 = Await.result(turn3on1.tryLock(), Duration.Zero).asInstanceOf[Locked].lock
    assert(Await.result(turn1.trySubsume(lock3on1), Duration.Zero) === Successful)

    assert(lock1on2.remotelyEquals(lock1))
    assert(lock1on2 !== lock1)
    assert(lock3on1.remotelyEquals(lock3))
    assert(lock3on1 !== lock3)
    assert(lock1.refCount.get === 1)    // lock1on2
    assert(lock1on2.refCount.get === 1) // turn2
    assert(lock2.refCount.get <= 0)
    assert(lock3.refCount.get === 2)    // turn3, lock3on1
    assert(lock3on1.refCount.get === 3) // turn1, lock1, thread

    val turn4 = host2.newTurn()
    turn4.beginExecuting()
    val lock4 = turn3.subsumableLock.get()
    val l4    = Await.result(turn4.tryLock(), Duration.Zero).asInstanceOf[Locked].lock
    assert(lock4.refCount.get === 2) // turn4, thread

    assert(Await.result(turn2.trySubsume(l4), Duration.Zero) === Blocked)

    assert(lock1.refCount.get <= 0)
    assert(lock1on2.refCount.get <= 0)
    assert(lock3.refCount.get === 2)    // turn3, lock3on1
    assert(lock3on1.refCount.get === 3) // turn1, lock3on2, thread
    val lock3on2 = host2.lockHost.getInstance(lock3.guid).get
    assert(lock3on2.refCount.get === 1) // turn2
    assert(lock4.refCount.get === 2)    // turn4, thread
  }

  test("remote indirect and direct other trySubsume works") {
    val host1 = new FullMVEngine(Duration.Zero, "host1")
    val host2 = new FullMVEngine(Duration.Zero, "host2")
    val host3 = new FullMVEngine(Duration.Zero, "host3")

    val turn1 = host1.newTurn()
    turn1.beginExecuting()
    val lock1 = turn1.subsumableLock.get()

    val turn2 = host2.newTurn()
    turn2.beginExecuting()
    val lock2 = turn2.subsumableLock.get()

    val turn3 = host3.newTurn()
    turn3.beginExecuting()
    val lock3 = turn3.subsumableLock.get()

    val turn1on2 = FullMVTurnLocalClone(turn1, host2)
    val lock1on2 = Await.result(turn1on2.tryLock(), Duration.Zero).asInstanceOf[Locked].lock
    assert(Await.result(turn2.trySubsume(lock1on2), Duration.Zero) === Successful)
    lock1on2.asyncUnlock()

    val turn3on1 = FullMVTurnLocalClone(turn3, host1)
    val lock3on1 = Await.result(turn3on1.tryLock(), Duration.Zero).asInstanceOf[Locked].lock
    assert(Await.result(turn1.trySubsume(lock3on1), Duration.Zero) === Successful)
    lock3on1.asyncUnlock()

    assert(lock1on2.remotelyEquals(lock1))
    assert(lock1on2 !== lock1)
    assert(lock3on1.remotelyEquals(lock3))
    assert(lock3on1 !== lock3)
    assert(lock1.refCount.get === 1)    // lock1on2
    assert(lock1on2.refCount.get === 1) // turn2
    assert(lock2.refCount.get <= 0)
    assert(lock3.refCount.get === 2)    // turn3, lock3on1
    assert(lock3on1.refCount.get === 2) // turn1, lock1

    val turn4 = host2.newTurn()
    turn4.beginExecuting()
    val lock4 = turn4.subsumableLock.get()
    val l4    = Await.result(turn4.tryLock(), Duration.Zero).asInstanceOf[Locked].lock
    assert(lock4.refCount.get === 2) // turn4, thread

    assert(Await.result(turn2.trySubsume(l4), Duration.Zero) === Successful)

    assert(lock1.refCount.get <= 0)
    assert(lock1on2.refCount.get <= 0)
    assert(lock3.refCount.get === 2)    // turn3, lock3on1
    assert(lock3on1.refCount.get === 1) // turn1
    assert(lock4.refCount.get === 4)    // turn2, turn4, lock4on1, thread
    val lock4on1 = host1.lockHost.getInstance(lock4.guid).get
    assert(lock4on1.refCount.get === 1) // lock4on3
    val lock4on3 = host3.lockHost.getInstance(lock4.guid).get
    assert(lock4on3.refCount.get === 1) // lock3
  }

  test("remote indirect trylock failure works") {
    val host1 = new FullMVEngine(Duration.Zero, "host1")
    val host2 = new FullMVEngine(Duration.Zero, "host2")

    val turn1 = host1.newTurn()
    turn1.beginExecuting()
    val lock1 = turn1.subsumableLock.get

    val turn2 = host2.newTurn()
    turn2.beginExecuting()
    val lock2 = turn2.subsumableLock.get

    val turn1on2 = FullMVTurnLocalClone(turn1, host2)
    val l2       = Await.result(turn2.tryLock(), Duration.Zero).asInstanceOf[Locked].lock
    assert(Await.result(turn1on2.trySubsume(l2), Duration.Zero) === Successful)
    l2.asyncUnlock()

    val turn3 = host2.newTurn()
    turn3.beginExecuting()
    val lock3 = turn3.subsumableLock.get

    val l3 = Await.result(turn3.tryLock(), Duration.Zero).asInstanceOf[Locked].lock
    assert(Await.result(turn2.trySubsume(l3), Duration.Zero) === Successful)

    assert(lock1.refCount.get <= 0)
    val lock2on1 = host1.lockHost.getInstance(lock2.guid).get
    assert(lock2on1.refCount.get === 1) // turn1
    assert(lock2.refCount.get === 1)    // lock2on1
    assert(lock3.refCount.get === 4)    // turn2, turn3, lock2, thread

    assert(Await.result(turn1.tryLock(), Duration.Zero) === Blocked)

    assert(lock1.refCount.get <= 0)
    assert(lock2on1.refCount.get <= 0)
    assert(lock2.refCount.get <= 0)
    val lock3on1 = host1.lockHost.getInstance(lock3.guid).get
    assert(lock3on1.refCount.get === 1) // turn1
    assert(lock3.refCount.get === 4)    // turn2, turn3, lock3on1, thread
  }

  test("remote indirect trylock success works") {
    val host1 = new FullMVEngine(Duration.Zero, "host1")
    val host2 = new FullMVEngine(Duration.Zero, "host2")

    val turn1 = host1.newTurn()
    turn1.beginExecuting()
    val lock1 = turn1.subsumableLock.get

    val turn2 = host2.newTurn()
    turn2.beginExecuting()
    val lock2 = turn2.subsumableLock.get

    val turn1on2 = FullMVTurnLocalClone(turn1, host2)
    val l2       = Await.result(turn2.tryLock(), Duration.Zero).asInstanceOf[Locked].lock
    assert(Await.result(turn1on2.trySubsume(l2), Duration.Zero) === Successful)
    l2.asyncUnlock()

    val turn3 = host2.newTurn()
    turn3.beginExecuting()
    val lock3 = turn3.subsumableLock.get

    val l3 = Await.result(turn3.tryLock(), Duration.Zero).asInstanceOf[Locked].lock
    assert(Await.result(turn2.trySubsume(l3), Duration.Zero) === Successful)
    l3.asyncUnlock()

    assert(lock1.refCount.get <= 0)
    val lock2on1 = host1.lockHost.getInstance(lock2.guid).get
    assert(lock2on1.refCount.get === 1) // turn1
    assert(lock2.refCount.get === 1)    // lock2on1
    assert(lock3.refCount.get === 3)    // turn2, turn3, lock2

    Await.result(turn1.tryLock(), Duration.Zero).asInstanceOf[Locked].lock

    assert(lock1.refCount.get <= 0)
    assert(lock2on1.refCount.get <= 0)
    assert(lock2.refCount.get <= 0)
    val lock3on1 = host1.lockHost.getInstance(lock3.guid).get
    assert(lock3on1.refCount.get === 2) // turn1, thread
    assert(lock3.refCount.get === 3)    // turn2, turn3, lock3on1
  }
}
