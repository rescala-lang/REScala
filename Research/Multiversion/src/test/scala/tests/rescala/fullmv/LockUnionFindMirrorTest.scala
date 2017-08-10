package tests.rescala.fullmv

import org.scalatest.FunSuite
import rescala.fullmv.mirrors.SubsumableLockLocalClone
import rescala.fullmv.sgt.synchronization.SubsumableLock.TryLockResult
import rescala.fullmv.sgt.synchronization.SubsumableLockImpl
import rescala.testhelper.Spawn

import scala.concurrent.TimeoutException

class LockUnionFindMirrorTest extends FunSuite {
  test("tryLock works"){
    // we can lock
    val a = new SubsumableLockImpl()
    val cloneA = SubsumableLockLocalClone(a)
    val cloneB = SubsumableLockLocalClone(a)
    assert(cloneA.tryLock() === TryLockResult(success = true, cloneA, a.gUID))

    // lock is exclusive
    assert(cloneB.tryLock() === TryLockResult(success = false, cloneB, a.gUID))

    // unlock works
    cloneA.unlock()
    assert(cloneB.tryLock() === TryLockResult(success = true, cloneB, a.gUID))
  }

  test("lock works") {
    // we can lock
    val a = new SubsumableLockImpl()
    val cloneA = SubsumableLockLocalClone(a)
    val cloneB = SubsumableLockLocalClone(a)
    assert(Spawn{cloneA.lock()}.join(101) === TryLockResult(success = true, cloneA, a.gUID))

    // lock is exclusive
    assert(cloneB.tryLock() === TryLockResult(success = false, cloneB, a.gUID))
    // and blocks..
    val blockedB = Spawn{cloneB.lock()}
    intercept[TimeoutException] {
      blockedB.join(103)
    }

    // unlock unblocks
    cloneA.unlock()
    assert(blockedB.join(104) === TryLockResult(success = true, cloneB, a.gUID))
  }

  test("union works") {
    val a = new SubsumableLockImpl
    val cloneA = SubsumableLockLocalClone(a)
    val b = new SubsumableLockImpl

    assert(cloneA.tryLock() === TryLockResult(success = true, cloneA, a.gUID))
    val resB = b.tryLock()
    assert(resB === TryLockResult(success = true, b, b.gUID))

    cloneA.subsume(resB)

    assert(a.getLockedRoot.contains(b.gUID))
    assert(cloneA.getLockedRoot.contains(b.gUID))
    assert(b.getLockedRoot.contains(b.gUID))

    cloneA.unlock()

    assert(cloneA.tryLock() === TryLockResult(success = true, cloneA, b.gUID))
    val resA = a.tryLock()
    assert(resA.success === false)
    assert(resA.globalRoot === b.gUID)
    assert(!Set(a, cloneA, b).contains(resA.newParent))
    assert(b.tryLock() === TryLockResult(success = false, b, b.gUID))

    b.unlock()

    assert(b.tryLock() === TryLockResult(success = true, b, b.gUID))
    assert(b.tryLock() === TryLockResult(success = false, b, b.gUID))
    assert(cloneA.tryLock() === TryLockResult(success = false, cloneA, b.gUID))
    val resAA = a.tryLock()
    assert(resAA.success === false)
    assert(resAA.globalRoot === b.gUID)
    assert(!Set(a, cloneA, b).contains(resAA.newParent))
  }

  test("trySubsume parameter does not increase multihops") {
    val a = new SubsumableLockImpl
    val cloneA = SubsumableLockLocalClone(a)
    val childOfCloneA = new SubsumableLockImpl
    val resA = cloneA.tryLock()
    assert(resA === TryLockResult(success = true, cloneA, a.gUID))
    assert(childOfCloneA.trySubsume(resA).isEmpty)
    cloneA.unlock()

    val b = new SubsumableLockImpl

    val resB = b.tryLock()
    assert(resB === TryLockResult(success = true, b, b.gUID))
    val resAA = childOfCloneA.trySubsume(resB)
    assert(resAA === None)

    assert(a.getLockedRoot.contains(b.gUID))
    assert(cloneA.getLockedRoot.contains(b.gUID))
    assert(childOfCloneA.getLockedRoot.contains(b.gUID))
    assert(b.getLockedRoot.contains(b.gUID))

    cloneA.unlock()

    assert(childOfCloneA.tryLock() === TryLockResult(success = true, b, b.gUID))
  }

  test("subsume parameter does not increase multihops") {
    val a = new SubsumableLockImpl
    val cloneA = SubsumableLockLocalClone(a)
    val childOfCloneA = new SubsumableLockImpl
    val resA = cloneA.tryLock()
    assert(resA === TryLockResult(success = true, cloneA, a.gUID))
    assert(childOfCloneA.trySubsume(resA).isEmpty)
    cloneA.unlock()

    val b = new SubsumableLockImpl

    assert(childOfCloneA.tryLock() === TryLockResult(success = true, cloneA, a.gUID))
    val resB = b.tryLock()
    assert(resB === TryLockResult(success = true, b, b.gUID))

    cloneA.subsume(resB)

    assert(a.getLockedRoot.contains(b.gUID))
    assert(cloneA.getLockedRoot.contains(b.gUID))
    assert(childOfCloneA.getLockedRoot.contains(b.gUID))
    assert(b.getLockedRoot.contains(b.gUID))

    cloneA.unlock()

    assert(childOfCloneA.tryLock() === TryLockResult(success = true, cloneA, b.gUID))
  }
}
