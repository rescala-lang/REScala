package tests.rescala.fullmv

import org.scalatest.FunSuite
import rescala.fullmv.mirrors.SubsumableLockHostImpl
import rescala.fullmv.sgt.synchronization.SubsumableLock.TryLockResult
import rescala.fullmv.sgt.synchronization.SubsumableLockImpl
import rescala.testhelper.Spawn

import scala.concurrent.duration.Duration
import scala.concurrent.{Await, TimeoutException}
import scala.util.{Failure, Try}

class LockUnionFindTest extends FunSuite {
  object host extends SubsumableLockHostImpl
  test("tryLock works"){
    // we can lock
    val a = host.newLock()
    assert(Await.result(a.tryLock(), Duration.Zero) === TryLockResult(success = true, a))

    // lock is exclusive
    assert(Await.result(a.tryLock(), Duration.Zero) === TryLockResult(success = false, a))

    // unlock works
    Await.result(a.unlock(), Duration.Zero)
    assert(Await.result(a.tryLock(), Duration.Zero) === TryLockResult(success = true, a))
  }

  test("lock works") {
    // we can lock
    val a = host.newLock()
    assert(Spawn{Await.result(a.lock(), Duration.Zero)}.join(101) === a)

    // lock is exclusive
    assert(Await.result(a.tryLock(), Duration.Zero) === TryLockResult(success = false, a))
    // and blocks..
    val blockedB = Spawn{Await.result(a.lock(), Duration.Zero)}
    intercept[TimeoutException] {
      blockedB.join(103)
    }

    // unlock unblocks
    Await.result(a.unlock(), Duration.Zero)
    assert(blockedB.join(104) === a)
  }

  test("union works") {
    val a = host.newLock()
    val b = host.newLock()

    assert(Await.result(a.tryLock(), Duration.Zero) === TryLockResult(success = true, a))
    val resB = Await.result(b.tryLock(), Duration.Zero)
    assert(resB === TryLockResult(success = true, b))

    Await.result(a.subsume(resB.newParent), Duration.Zero)

    assert(Await.result(a.getLockedRoot, Duration.Zero).contains(b.guid))
    assert(Await.result(b.getLockedRoot, Duration.Zero).contains(b.guid))

    Await.result(b.unlock(), Duration.Zero)

    assert(Await.result(a.tryLock(), Duration.Zero) === TryLockResult(success = true, b))
    assert(Await.result(a.tryLock(), Duration.Zero) === TryLockResult(success = false, b))
    assert(Await.result(b.tryLock(), Duration.Zero) === TryLockResult(success = false, b))

    Await.result(b.unlock(), Duration.Zero)

    assert(Await.result(b.tryLock(), Duration.Zero) === TryLockResult(success = true, b))
    assert(Await.result(b.tryLock(), Duration.Zero) === TryLockResult(success = false, b))
    assert(Await.result(a.tryLock(), Duration.Zero) === TryLockResult(success = false, b))
  }

  test("subsume correctly wakes all threads") {
    val a, b = host.newLock()
    assert(Await.result(a.tryLock(), Duration.Zero).success)
    val resB = Await.result(b.tryLock(), Duration.Zero)
    assert(resB.success)

    var counter = 0
    def spawnIncrementUnderLockThread(lock: SubsumableLockImpl) = {
      Spawn {
        val newParent = Await.result(lock.lock(), Duration.Zero)
        val c = counter
        counter += 1
        Await.result(newParent.unlock(), Duration.Zero)
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

    Await.result(a.subsume(resB.newParent), Duration.Zero)
    Await.result(resB.newParent.unlock(), Duration.Zero)

    assert(queued.map(_.join(50)).toSet === (0 until 10).map(_ -> resB.newParent.guid).toSet)
  }
}
