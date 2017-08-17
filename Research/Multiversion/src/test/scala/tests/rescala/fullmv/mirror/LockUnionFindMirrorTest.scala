package tests.rescala.fullmv.mirror

import org.scalatest.FunSuite
import rescala.fullmv.mirrors.SubsumableLockHostImpl
import rescala.fullmv.sgt.synchronization.SubsumableLock.TryLockResult
import rescala.testhelper.Spawn
import rescala.fullmv.mirrors.localcloning.SubsumableLockLocalClone

import scala.concurrent.TimeoutException

class LockUnionFindMirrorTest extends FunSuite {
  val host0, hostA, hostB = new SubsumableLockHostImpl
  test("instance lookup and equality"){
    val a = host0.newLock()
    assert((SubsumableLockLocalClone(a, host0) eq a) === true)

    val cloneA = SubsumableLockLocalClone(a, hostA)
    assert((cloneA ne a) === true)
    assert(cloneA === a)
    assert((SubsumableLockLocalClone(a, hostA) eq cloneA) === true)
    assert((SubsumableLockLocalClone(cloneA, hostA) eq cloneA) === true)
    assert((SubsumableLockLocalClone(cloneA, host0) eq a) === true)

    val cloneB = SubsumableLockLocalClone(cloneA, hostB)
    assert((cloneB ne a) === true)
    assert(cloneB === a)
    assert((cloneB ne cloneA) === true)
    assert(cloneB === cloneA)
    assert((SubsumableLockLocalClone(cloneA, hostB) eq cloneB) === true)
    assert((SubsumableLockLocalClone(cloneB, hostB) eq cloneB) === true)
    assert((SubsumableLockLocalClone(cloneB, hostA) eq cloneA) === true)
    assert((SubsumableLockLocalClone(cloneB, host0) eq a) === true)
  }

  test("tryLock works"){
    // we can lock
    val a = host0.newLock()
    val cloneA = SubsumableLockLocalClone(a, hostA)
    val cloneB = SubsumableLockLocalClone(a, hostB)
    assert(cloneA.tryLock() === TryLockResult(success = true, cloneA))

    // lock is exclusive
    assert(cloneB.tryLock() === TryLockResult(success = false, cloneB))

    // unlock works
    cloneA.unlock()
    assert(cloneB.tryLock() === TryLockResult(success = true, cloneB))
  }

  test("lock works") {
    // we can lock
    val a = host0.newLock()
    val cloneA = SubsumableLockLocalClone(a, hostA)
    val cloneB = SubsumableLockLocalClone(a, hostB)
    assert(Spawn{cloneA.lock()}.join(101) === cloneA)

    // lock is exclusive
    assert(cloneB.tryLock() === TryLockResult(success = false, cloneB))
    // and blocks..
    val blockedB = Spawn{cloneB.lock()}
    intercept[TimeoutException] {
      blockedB.join(103)
    }

    // unlock unblocks
    cloneA.unlock()
    assert(blockedB.join(104) === cloneB)
  }

  test("union works") {
    val lockOneOn0 = host0.newLock()
    val lockOneOnA = SubsumableLockLocalClone(lockOneOn0, hostA)
    val lockTwoOnA = hostA.newLock()

    val resOneOnA = lockOneOnA.tryLock()
    assert(resOneOnA === TryLockResult(success = true, lockOneOnA))
    assert((resOneOnA.newParent eq lockOneOnA) === true)
    val resTwoOnA = lockTwoOnA.tryLock()
    assert(resTwoOnA === TryLockResult(success = true, lockTwoOnA))
    assert((resTwoOnA.newParent eq lockTwoOnA) === true)

    lockOneOnA.subsume(resTwoOnA.newParent)

    assert(lockOneOn0.getLockedRoot.contains(lockTwoOnA.guid))
    assert(lockOneOnA.getLockedRoot.contains(lockTwoOnA.guid))
    assert(lockTwoOnA.getLockedRoot.contains(lockTwoOnA.guid))

    lockOneOnA.unlock()

    val resOneOnA2 = lockOneOnA.tryLock()
    assert(resOneOnA2 === TryLockResult(success = true, lockTwoOnA))
    assert((resOneOnA2.newParent eq lockTwoOnA) === true)

    val resOneOn0 = lockOneOn0.tryLock()
    assert(resOneOn0 === TryLockResult(success = false, lockTwoOnA))
    assert((resOneOn0.newParent ne lockTwoOnA) === true)
    val resTwoOnA2 = lockTwoOnA.tryLock()
    assert(resTwoOnA2 === TryLockResult(success = false, lockTwoOnA))
    assert((resTwoOnA2.newParent eq lockTwoOnA) === true)

    lockTwoOnA.unlock()

    val resTwoOnA3 = lockTwoOnA.tryLock()
    assert(resTwoOnA3 === TryLockResult(success = true, lockTwoOnA))
    assert((resTwoOnA3.newParent eq lockTwoOnA) === true)
  }

  test("trySubsume parameter does not increase multihops") {
    val a = host0.newLock()
    val cloneA = SubsumableLockLocalClone(a, hostA)
    val childOfCloneA = hostA.newLock()
    val resA = cloneA.tryLock()
    assert(resA === TryLockResult(success = true, cloneA))
    assert(childOfCloneA.trySubsume(resA.newParent).isEmpty)
    cloneA.unlock()

    val b = hostA.newLock()

    val resB = b.tryLock()
    assert(resB === TryLockResult(success = true, b))
    val resAA = childOfCloneA.trySubsume(resB.newParent)
    assert(resAA === None)

    assert(a.getLockedRoot.contains(b.guid))
    assert(cloneA.getLockedRoot.contains(b.guid))
    assert(childOfCloneA.getLockedRoot.contains(b.guid))
    assert(b.getLockedRoot.contains(b.guid))

    cloneA.unlock()

    assert(childOfCloneA.tryLock() === TryLockResult(success = true, b))
  }

  test("subsume parameter does not increase multihops") {
    val a = host0.newLock()
    val cloneA = SubsumableLockLocalClone(a, hostA)
    val childOfCloneA = hostA.newLock()
    val resA = cloneA.tryLock()
    assert(resA === TryLockResult(success = true, cloneA))
    assert(childOfCloneA.trySubsume(resA.newParent).isEmpty)
    cloneA.unlock()

    val b = hostA.newLock()

    assert(childOfCloneA.tryLock() === TryLockResult(success = true, cloneA))
    val resB = b.tryLock()
    assert(resB === TryLockResult(success = true, b))

    cloneA.subsume(resB.newParent)

    assert(a.getLockedRoot.contains(b.guid))
    assert(cloneA.getLockedRoot.contains(b.guid))
    assert(childOfCloneA.getLockedRoot.contains(b.guid))
    assert(b.getLockedRoot.contains(b.guid))

    cloneA.unlock()

    val res = childOfCloneA.tryLock()
    assert(res === TryLockResult(success = true, b))
    assert((res.newParent eq b) === true)
  }
}
