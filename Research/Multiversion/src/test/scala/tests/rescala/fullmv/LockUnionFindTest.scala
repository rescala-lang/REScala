package tests.rescala.fullmv

import org.scalatest.FunSuite
import rescala.fullmv.mirrors.SubsumableLockHostImpl
import rescala.fullmv.sgt.synchronization.SubsumableLock.TryLockResult
import rescala.fullmv.sgt.synchronization.SubsumableLockImpl
import rescala.testhelper.Spawn

import scala.concurrent.TimeoutException
import scala.util.{Failure, Try}

class LockUnionFindTest extends FunSuite {
  object host extends SubsumableLockHostImpl
  test("tryLock works"){
    // we can lock
    val a = host.newLock()
    assert(a.tryLock() === TryLockResult(success = true, a))

    // lock is exclusive
    assert(a.tryLock() === TryLockResult(success = false, a))

    // unlock works
    a.unlock()
    assert(a.tryLock() === TryLockResult(success = true, a))
  }

  test("lock works") {
    // we can lock
    val a = host.newLock()
    assert(Spawn{a.lock()}.join(101) === a)

    // lock is exclusive
    assert(a.tryLock() === TryLockResult(success = false, a))
    // and blocks..
    val blockedB = Spawn{a.lock()}
    intercept[TimeoutException] {
      blockedB.join(103)
    }

    // unlock unblocks
    a.unlock()
    assert(blockedB.join(104) === a)
  }

  test("union works") {
    val a = host.newLock()
    val b = host.newLock()

    assert(a.tryLock() === TryLockResult(success = true, a))
    val resB = b.tryLock()
    assert(resB === TryLockResult(success = true, b))

    a.subsume(resB.newParent)

    assert(a.getLockedRoot.contains(b.guid))
    assert(b.getLockedRoot.contains(b.guid))

    b.unlock()

    assert(a.tryLock() === TryLockResult(success = true, b))
    assert(a.tryLock() === TryLockResult(success = false, b))
    assert(b.tryLock() === TryLockResult(success = false, b))

    b.unlock()

    assert(b.tryLock() === TryLockResult(success = true, b))
    assert(b.tryLock() === TryLockResult(success = false, b))
    assert(a.tryLock() === TryLockResult(success = false, b))
  }

  test("subsume correctly wakes all threads") {
    val a, b = host.newLock()
    assert(a.tryLock().success)
    val resB = b.tryLock()
    assert(resB.success)

    var counter = 0
    def spawnIncrementUnderLockThread(lock: SubsumableLockImpl) = {
      Spawn {
        val newParent = lock.lock()
        val c = counter
        counter += 1
        newParent.unlock()
        c -> newParent.guid
      }
    }

    val queued = List.fill(5){ spawnIncrementUnderLockThread(a) } ++ List.fill(5){ spawnIncrementUnderLockThread(b) }

    val timeout = System.currentTimeMillis() + 50
    val timeouts = queued.map { thread => Try { thread.join(timeout - System.currentTimeMillis()) } }
    assert(!timeouts.exists{
      case Failure(_: TimeoutException) => false
      case _ => true
    }, s"All threads should have timed out, but some did not.")

    a.subsume(resB.newParent)
    resB.newParent.unlock()

    assert(queued.map(_.join(50)).toSet === (0 until 10).map(_ -> resB.newParent.guid).toSet)
  }
}
