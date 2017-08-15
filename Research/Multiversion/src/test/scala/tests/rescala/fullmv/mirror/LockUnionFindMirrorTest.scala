package tests.rescala.fullmv.mirror

import org.scalatest.FunSuite
import rescala.fullmv.mirrors.SubsumableLockHostImpl
import rescala.fullmv.sgt.synchronization.SubsumableLock.TryLockResult
import rescala.testhelper.Spawn
import rescala.fullmv.mirrors.localcloning.SubsumableLockLocalClone

import scala.concurrent.TimeoutException

class LockUnionFindMirrorTest extends FunSuite {
  val host0, hostA, hostB = new SubsumableLockHostImpl {}
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
    assert(cloneA.tryLock() === TryLockResult(success = true, cloneA, a.guid))

    // lock is exclusive
    assert(cloneB.tryLock() === TryLockResult(success = false, cloneB, a.guid))

    // unlock works
    cloneA.unlock()
    assert(cloneB.tryLock() === TryLockResult(success = true, cloneB, a.guid))
  }

  test("lock works") {
    // we can lock
    val a = host0.newLock()
    val cloneA = SubsumableLockLocalClone(a, hostA)
    val cloneB = SubsumableLockLocalClone(a, hostB)
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
    val a = host0.newLock()
    val cloneA = SubsumableLockLocalClone(a, hostA)
    val b = hostA.newLock()

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
    assert(resA.newParent === b)
    assert(resA.newParent ne b)
    assert(b.tryLock() === TryLockResult(success = false, b, b.guid))

    b.unlock()

    assert(b.tryLock() === TryLockResult(success = true, b, b.guid))
    assert(b.tryLock() === TryLockResult(success = false, b, b.guid))
    assert(cloneA.tryLock() === TryLockResult(success = false, cloneA, b.guid))
    val resAA = a.tryLock()
    assert(resAA.success === false)
    assert(resAA.globalRoot === b.guid)
    assert(resA.newParent === b)
    assert(resA.newParent ne b)
  }

  test("trySubsume parameter does not increase multihops") {
    val a = host0.newLock()
    val cloneA = SubsumableLockLocalClone(a, hostA)
    val childOfCloneA = hostA.newLock()
    val resA = cloneA.tryLock()
    assert(resA === TryLockResult(success = true, cloneA, a.guid))
    assert(childOfCloneA.trySubsume(resA).isEmpty)
    cloneA.unlock()

    val b = hostA.newLock()

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
    val a = host0.newLock()
    val cloneA = SubsumableLockLocalClone(a, hostA)
    val childOfCloneA = hostA.newLock()
    val resA = cloneA.tryLock()
    assert(resA === TryLockResult(success = true, cloneA, a.guid))
    assert(childOfCloneA.trySubsume(resA).isEmpty)
    cloneA.unlock()

    val b = hostA.newLock()

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
