package tests.rescala.fullmv

import org.scalatest.FunSuite
import rescala.fullmv.sgt.synchronization.SubsumableLock.TryLockResult
import rescala.fullmv.sgt.synchronization.SubsumableLockImpl
import rescala.testhelper.Spawn

import scala.concurrent.TimeoutException

class LockUnionFindTest extends FunSuite {
  test("tryLock works"){
    // we can lock
    val a = new SubsumableLockImpl()
    assert(a.tryLock() === TryLockResult(success = true, a))

    // lock is exclusive
    assert(a.tryLock() === TryLockResult(success = false, a))

    // unlock works
    a.unlock()
    assert(a.tryLock() === TryLockResult(success = true, a))
  }

  test("lock works") {
    // we can lock
    val a = new SubsumableLockImpl()
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
    val a = new SubsumableLockImpl
    val b = new SubsumableLockImpl

    assert(a.tryLock() === TryLockResult(success = true, a))
    assert(b.tryLock() === TryLockResult(success = true, b))

    val ab = a.subsume(b)

    ab.unlock()

    assert(a.tryLock() === TryLockResult(success = true, ab))
    assert(a.tryLock() === TryLockResult(success = false, ab))
    assert(b.tryLock() === TryLockResult(success = false, ab))

    ab.unlock()

    assert(b.tryLock() === TryLockResult(success = true, ab))
    assert(b.tryLock() === TryLockResult(success = false, ab))
    assert(a.tryLock() === TryLockResult(success = false, ab))
  }

  test("subsume correctly wakes all threads") {
    val a, b = new SubsumableLockImpl()
    assert(a.tryLock().success)
    assert(b.tryLock().success)

    var counter = 0

    val queued = List.fill(5){
      Spawn {
        val r = a.lock()
        val res = counter
        counter += 1
        r.unlock()
        res
      }
    } ++ List.fill(5){
      Spawn {
        val r = b.lock()
        val res = counter
        counter += 1
        r.unlock()
        res
      }
    }

    val timeout = System.currentTimeMillis() + 50
    queued.foreach { thread =>
      intercept[TimeoutException]{
        thread.join(timeout - System.currentTimeMillis())
      }
    }

    val master = a.subsume(b)
    master.unlock()

    assert(queued.map(_.join(50)).toSet === (0 until 10).toSet)
  }
}
