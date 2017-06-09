package tests.rescala.fullmv

import org.scalatest.FunSuite
import rescala.fullmv.sgt.synchronization.SubsumableLock.TryLockResult
import rescala.fullmv.sgt.synchronization.SubsumableLockImpl
import rescala.testhelper.Spawn

import scala.concurrent.TimeoutException

class LockUnionFindTest extends FunSuite {
  test("tryLock works"){
    // we can lock
    object keyA
    val a = new SubsumableLockImpl()
    assert(a.tryLock(keyA) === TryLockResult(success = true, a))

    // lock is reentrant
    assert(a.tryLock(keyA) === TryLockResult(success = true, a))

    // lock is exclusive
    object keyB
    assert(a.tryLock(keyB) === TryLockResult(success = false, a))

    // unlock is exclusive
    intercept[AssertionError]{
      a.unlock(keyB)
    }
    assert(a.tryLock(keyB) === TryLockResult(success = false, a))

    // correct unlock works
    a.unlock(keyA)
    assert(a.tryLock(keyB) === TryLockResult(success = true, a))
  }

  test("lock works") {
    // we can lock
    object keyA
    val a = new SubsumableLockImpl()
    assert(Spawn{a.lock(keyA)}.join(101) === a)

    // lock is reentrant
    assert(a.tryLock(keyA) === TryLockResult(success = true, a))
    assert(Spawn{a.lock(keyA)}.join(102) === a)

    // lock is exclusive
    object keyB
    assert(a.tryLock(keyB) === TryLockResult(success = false, a))
    // and blocks..
    val blockedB = Spawn{a.lock(keyB)}
    intercept[TimeoutException] {
      blockedB.join(103)
    }

    // unlock unblocks
    a.unlock(keyA)
    assert(blockedB.join(104) === a)
  }

  test("union works") {
   object keyA
    val a = new SubsumableLockImpl
    val b = new SubsumableLockImpl

    assert(a.tryLock(keyA) === TryLockResult(success = true, a))
    assert(b.tryLock(keyA) === TryLockResult(success = true, b))

    val ab = a.subsume(b)

    ab.unlock(keyA)

    assert(a.tryLock(keyA) === TryLockResult(success = true, ab))
    object keyB
    assert(a.tryLock(keyB) === TryLockResult(success = false, ab))
    assert(b.tryLock(keyB) === TryLockResult(success = false, ab))
    intercept[AssertionError] {
      ab.unlock(keyB)
    }
    assert(b.tryLock(keyA) === TryLockResult(success = true, ab))

    ab.unlock(keyA)

    assert(b.tryLock(keyB) === TryLockResult(success = true, ab))
    assert(b.tryLock(keyA) === TryLockResult(success = false, ab))
    assert(a.tryLock(keyA) === TryLockResult(success = false, ab))
    assert(a.tryLock(keyB) === TryLockResult(success = true, ab))
  }

  test("subsume correctly wakes all threads") {
    object key
    val a, b = new SubsumableLockImpl()
    assert(a.tryLock(key).success)
    assert(b.tryLock(key).success)

    var counter = 0

    val queued = List.fill(5){
      Spawn {
        object key
        val r = a.lock(key)
        val res = counter
        counter += 1
        r.unlock(key)
        res
      }
    } ++ List.fill(5){
      Spawn {
        object key
        val r = b.lock(key)
        val res = counter
        counter += 1
        r.unlock(key)
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
    master.unlock(key)

    assert(queued.map(_.join(50)).toSet === (0 until 10).toSet)
  }
}
