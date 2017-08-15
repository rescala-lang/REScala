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
    assert(a.tryLock() === TryLockResult(success = true, a, a.guid))

    // lock is exclusive
    assert(a.tryLock() === TryLockResult(success = false, a, a.guid))

    // unlock works
    a.unlock()
    assert(a.tryLock() === TryLockResult(success = true, a, a.guid))
  }

  test("lock works") {
    // we can lock
    val a = host.newLock()
    assert(Spawn{a.lock()}.join(101) === TryLockResult(success = true, a, a.guid))

    // lock is exclusive
    assert(a.tryLock() === TryLockResult(success = false, a, a.guid))
    // and blocks..
    val blockedB = Spawn{a.lock()}
    intercept[TimeoutException] {
      blockedB.join(103)
    }

    // unlock unblocks
    a.unlock()
    assert(blockedB.join(104) === TryLockResult(success = true, a, a.guid))
  }

  test("union works") {
    val a = host.newLock()
    val b = host.newLock()

    assert(a.tryLock() === TryLockResult(success = true, a, a.guid))
    val resB = b.tryLock()
    assert(resB === TryLockResult(success = true, b, b.guid))

    a.subsume(resB)

    assert(a.getLockedRoot.contains(b.guid))
    assert(b.getLockedRoot.contains(b.guid))

    b.unlock()

    assert(a.tryLock() === TryLockResult(success = true, b, b.guid))
    assert(a.tryLock() === TryLockResult(success = false, b, b.guid))
    assert(b.tryLock() === TryLockResult(success = false, b, b.guid))

    b.unlock()

    assert(b.tryLock() === TryLockResult(success = true, b, b.guid))
    assert(b.tryLock() === TryLockResult(success = false, b, b.guid))
    assert(a.tryLock() === TryLockResult(success = false, b, b.guid))
  }

  test("subsume correctly wakes all threads") {
    val a, b = host.newLock()
    assert(a.tryLock().success)
    val resB = b.tryLock()
    assert(resB.success)

    var counter = 0
    def spawnIncrementUnderLockThread(lock: SubsumableLockImpl) = {
      Spawn {
        val res = lock.lock()
        assert(res.success)
        val c = counter
        counter += 1
        res.newParent.unlock()
        c -> res.globalRoot
      }
    }

    val queued = List.fill(5){ spawnIncrementUnderLockThread(a) } ++ List.fill(5){ spawnIncrementUnderLockThread(b) }

    val timeout = System.currentTimeMillis() + 50
    val timeouts = queued.map { thread => Try { thread.join(timeout - System.currentTimeMillis()) } }
    assert(!timeouts.exists{
      case Failure(_: TimeoutException) => false
      case _ => true
    }, s"All threads should have timed out, but some did not.")

    a.subsume(resB)
    resB.newParent.unlock()

    assert(queued.map(_.join(50)).toSet === (0 until 10).map(_ -> resB.globalRoot).toSet)
  }
}
