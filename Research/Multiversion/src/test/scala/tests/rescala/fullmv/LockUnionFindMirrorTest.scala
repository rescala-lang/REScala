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
    assert(cloneA.tryLock() === TryLockResult(success = true, cloneA, a.guid))

    // lock is exclusive
    assert(cloneB.tryLock() === TryLockResult(success = false, cloneB, a.guid))

    // unlock works
    cloneA.unlock()
    assert(cloneB.tryLock() === TryLockResult(success = true, cloneB, a.guid))
  }

  test("lock works") {
    // we can lock
    val a = new SubsumableLockImpl()
    val cloneA = SubsumableLockLocalClone(a)
    val cloneB = SubsumableLockLocalClone(a)
    assert(Spawn{cloneA.lock()}.join(101) === TryLockResult(success = true, cloneA, a.guid))

    // lock is exclusive
    assert(cloneB.tryLock() === TryLockResult(success = false, cloneB, a.guid))
    // and blocks..
    val blockedB = Spawn{cloneB.lock()}
    intercept[TimeoutException] {
      blockedB.join(103)
    }

    // unlock unblocks
    cloneA.unlock()
    assert(blockedB.join(104) === TryLockResult(success = true, cloneB, a.guid))
  }

  test("union works") {
    val a = new SubsumableLockImpl
    val cloneA = SubsumableLockLocalClone(a)
    val b = new SubsumableLockImpl

    assert(cloneA.tryLock() === TryLockResult(success = true, cloneA, a.guid))
    val resB = b.tryLock()
    assert(resB === TryLockResult(success = true, b, b.guid))

    cloneA.subsume(resB)

    assert(a.getLockedRoot.contains(b.guid))
    assert(cloneA.getLockedRoot.contains(b.guid))
    assert(b.getLockedRoot.contains(b.guid))

    cloneA.unlock()

    assert(cloneA.tryLock() === TryLockResult(success = true, cloneA, b.guid))
    val resA = a.tryLock()
    assert(resA.success === false)
    assert(resA.globalRoot === b.guid)
    assert(!Set(a, cloneA, b).contains(resA.newParent))
    assert(b.tryLock() === TryLockResult(success = false, b, b.guid))

    b.unlock()

    assert(b.tryLock() === TryLockResult(success = true, b, b.guid))
    assert(b.tryLock() === TryLockResult(success = false, b, b.guid))
    assert(cloneA.tryLock() === TryLockResult(success = false, cloneA, b.guid))
    val resAA = a.tryLock()
    assert(resAA.success === false)
    assert(resAA.globalRoot === b.guid)
    assert(!Set(a, cloneA, b).contains(resAA.newParent))
  }

  test("trySubsume parameter does not increase multihops") {
    val a = new SubsumableLockImpl
    val cloneA = SubsumableLockLocalClone(a)
    val childOfCloneA = new SubsumableLockImpl
    val resA = cloneA.tryLock()
    assert(resA === TryLockResult(success = true, cloneA, a.guid))
    assert(childOfCloneA.trySubsume(resA).isEmpty)
    cloneA.unlock()

    val b = new SubsumableLockImpl

    val resB = b.tryLock()
    assert(resB === TryLockResult(success = true, b, b.guid))
    val resAA = childOfCloneA.trySubsume(resB)
    assert(resAA === None)

    assert(a.getLockedRoot.contains(b.guid))
    assert(cloneA.getLockedRoot.contains(b.guid))
    assert(childOfCloneA.getLockedRoot.contains(b.guid))
    assert(b.getLockedRoot.contains(b.guid))

    cloneA.unlock()

    assert(childOfCloneA.tryLock() === TryLockResult(success = true, b, b.guid))
  }

  test("subsume parameter does not increase multihops") {
    val a = new SubsumableLockImpl
    val cloneA = SubsumableLockLocalClone(a)
    val childOfCloneA = new SubsumableLockImpl
    val resA = cloneA.tryLock()
    assert(resA === TryLockResult(success = true, cloneA, a.guid))
    assert(childOfCloneA.trySubsume(resA).isEmpty)
    cloneA.unlock()

    val b = new SubsumableLockImpl

    assert(childOfCloneA.tryLock() === TryLockResult(success = true, cloneA, a.guid))
    val resB = b.tryLock()
    assert(resB === TryLockResult(success = true, b, b.guid))

    cloneA.subsume(resB)

    assert(a.getLockedRoot.contains(b.guid))
    assert(cloneA.getLockedRoot.contains(b.guid))
    assert(childOfCloneA.getLockedRoot.contains(b.guid))
    assert(b.getLockedRoot.contains(b.guid))

    cloneA.unlock()

    assert(childOfCloneA.tryLock() === TryLockResult(success = true, cloneA, b.guid))
  }
}
