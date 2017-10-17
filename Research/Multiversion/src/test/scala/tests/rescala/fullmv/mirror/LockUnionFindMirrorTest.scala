//package tests.rescala.fullmv.mirror
//
//import org.scalatest.FunSuite
//import rescala.fullmv.mirrors.SubsumableLockHostImpl
//import rescala.fullmv.sgt.synchronization.SubsumableLock.TryLockResult
//import rescala.testhelper.Spawn
//import rescala.fullmv.mirrors.localcloning.SubsumableLockLocalClone
//
//import scala.concurrent.duration.Duration
//import scala.concurrent.{Await, TimeoutException}
//
//class LockUnionFindMirrorTest extends FunSuite {
//  val host0, hostA, hostB = new SubsumableLockHostImpl
//  test("instance lookup and equality"){
//    val a = host0.newLock()
//    assert((SubsumableLockLocalClone(a, host0) eq a) === true)
//
//    val cloneA = SubsumableLockLocalClone(a, hostA)
//    assert((cloneA ne a) === true)
//    assert(cloneA === a)
//    assert((SubsumableLockLocalClone(a, hostA) eq cloneA) === true)
//    assert((SubsumableLockLocalClone(cloneA, hostA) eq cloneA) === true)
//    assert((SubsumableLockLocalClone(cloneA, host0) eq a) === true)
//
//    val cloneB = SubsumableLockLocalClone(cloneA, hostB)
//    assert((cloneB ne a) === true)
//    assert(cloneB === a)
//    assert((cloneB ne cloneA) === true)
//    assert(cloneB === cloneA)
//    assert((SubsumableLockLocalClone(cloneA, hostB) eq cloneB) === true)
//    assert((SubsumableLockLocalClone(cloneB, hostB) eq cloneB) === true)
//    assert((SubsumableLockLocalClone(cloneB, hostA) eq cloneA) === true)
//    assert((SubsumableLockLocalClone(cloneB, host0) eq a) === true)
//  }
//
//  test("tryLock works"){
//    // we can lock
//    val a = host0.newLock()
//    val cloneA = SubsumableLockLocalClone(a, hostA)
//    val cloneB = SubsumableLockLocalClone(a, hostB)
//    assert(Await.result(cloneA.tryLock(), Duration.Zero) === TryLockResult(success = true, cloneA))
//
//    // lock is exclusive
//    assert(Await.result(cloneB.tryLock(), Duration.Zero) === TryLockResult(success = false, cloneB))
//
//    // unlock works
//    Await.result(cloneA.unlock(), Duration.Zero)
//    assert(Await.result(cloneB.tryLock(), Duration.Zero) === TryLockResult(success = true, cloneB))
//  }
//
//  test("lock works") {
//    // we can lock
//    val a = host0.newLock()
//    val cloneA = SubsumableLockLocalClone(a, hostA)
//    val cloneB = SubsumableLockLocalClone(a, hostB)
//    assert(Spawn{Await.result(cloneA.lock(), Duration.Zero)}.join(101) === cloneA)
//
//    // lock is exclusive
//    assert(Await.result(cloneB.tryLock(), Duration.Zero) === TryLockResult(success = false, cloneB))
//    // and blocks..
//    val blockedB = Spawn{Await.result(cloneB.lock(), Duration.Zero)}
//    intercept[TimeoutException] {
//      blockedB.join(103)
//    }
//
//    // unlock unblocks
//    Await.result(cloneA.unlock(), Duration.Zero)
//    assert(blockedB.join(104) === cloneB)
//  }
//
//  test("union works") {
//    val lockOneOn0 = host0.newLock()
//    val lockOneOnA = SubsumableLockLocalClone(lockOneOn0, hostA)
//    val lockTwoOnA = hostA.newLock()
//
//    val resOneOnA = Await.result(lockOneOnA.tryLock(), Duration.Zero)
//    assert(resOneOnA === TryLockResult(success = true, lockOneOnA))
//    assert((resOneOnA.newParent eq lockOneOnA) === true)
//    val resTwoOnA = Await.result(lockTwoOnA.tryLock(), Duration.Zero)
//    assert(resTwoOnA === TryLockResult(success = true, lockTwoOnA))
//    assert((resTwoOnA.newParent eq lockTwoOnA) === true)
//
//    Await.result(lockOneOnA.subsume(resTwoOnA.newParent), Duration.Zero)
//
//    assert(Await.result(lockOneOn0.getLockedRoot, Duration.Zero).contains(lockTwoOnA.guid))
//    assert(Await.result(lockOneOnA.getLockedRoot, Duration.Zero).contains(lockTwoOnA.guid))
//    assert(Await.result(lockTwoOnA.getLockedRoot, Duration.Zero).contains(lockTwoOnA.guid))
//
//    Await.result(resTwoOnA.newParent.unlock(), Duration.Zero)
//
//    val resOneOnA2 = Await.result(lockOneOnA.tryLock(), Duration.Zero)
//    assert(resOneOnA2 === TryLockResult(success = true, lockTwoOnA))
//    assert((resOneOnA2.newParent eq lockTwoOnA) === true)
//
//    val resOneOn0 = Await.result(lockOneOn0.tryLock(), Duration.Zero)
//    assert(resOneOn0 === TryLockResult(success = false, lockTwoOnA))
//    assert((resOneOn0.newParent ne lockTwoOnA) === true)
//    val resTwoOnA2 = Await.result(lockTwoOnA.tryLock(), Duration.Zero)
//    assert(resTwoOnA2 === TryLockResult(success = false, lockTwoOnA))
//    assert((resTwoOnA2.newParent eq lockTwoOnA) === true)
//
//    Await.result(lockTwoOnA.unlock(), Duration.Zero)
//
//    val resTwoOnA3 = Await.result(lockTwoOnA.tryLock(), Duration.Zero)
//    assert(resTwoOnA3 === TryLockResult(success = true, lockTwoOnA))
//    assert((resTwoOnA3.newParent eq lockTwoOnA) === true)
//  }
//
//  test("trySubsume parameter does not increase multihops") {
//    val a = host0.newLock()
//    val cloneA = SubsumableLockLocalClone(a, hostA)
//    val childOfCloneA = hostA.newLock()
//    val resA = Await.result(cloneA.tryLock(), Duration.Zero)
//    assert(resA === TryLockResult(success = true, cloneA))
//    assert(Await.result(childOfCloneA.trySubsume(resA.newParent), Duration.Zero).isEmpty)
//    Await.result(cloneA.unlock(), Duration.Zero)
//
//    val b = hostA.newLock()
//
//    val resB = Await.result( b.tryLock(), Duration.Zero)
//    assert(resB === TryLockResult(success = true, b))
//    val resAA = Await.result(childOfCloneA.trySubsume(resB.newParent), Duration.Zero)
//    assert(resAA === None)
//
//    assert(Await.result(a.getLockedRoot, Duration.Zero).contains(b.guid))
//    assert(Await.result(cloneA.getLockedRoot, Duration.Zero).contains(b.guid))
//    assert(Await.result(childOfCloneA.getLockedRoot, Duration.Zero).contains(b.guid))
//    assert(Await.result(b.getLockedRoot, Duration.Zero).contains(b.guid))
//
//    Await.result(resB.newParent.unlock(), Duration.Zero)
//
//    assert(Await.result(childOfCloneA.tryLock(), Duration.Zero) === TryLockResult(success = true, b))
//  }
//
//  test("subsume parameter does not increase multihops") {
//    val a = host0.newLock()
//    val cloneA = SubsumableLockLocalClone(a, hostA)
//    val childOfCloneA = hostA.newLock()
//    val resA = Await.result(cloneA.tryLock(), Duration.Zero)
//    assert(resA === TryLockResult(success = true, cloneA))
//    assert(Await.result(childOfCloneA.trySubsume(resA.newParent), Duration.Zero).isEmpty)
//    Await.result(cloneA.unlock(), Duration.Zero)
//
//    val b = hostA.newLock()
//
//    assert(Await.result(childOfCloneA.tryLock(), Duration.Zero) === TryLockResult(success = true, cloneA))
//    val resB = Await.result(b.tryLock(), Duration.Zero)
//    assert(resB === TryLockResult(success = true, b))
//
//    Await.result(cloneA.subsume(resB.newParent), Duration.Zero)
//
//    assert(Await.result(a.getLockedRoot, Duration.Zero).contains(b.guid))
//    assert(Await.result(cloneA.getLockedRoot, Duration.Zero).contains(b.guid))
//    assert(Await.result(childOfCloneA.getLockedRoot, Duration.Zero).contains(b.guid))
//    assert(Await.result(b.getLockedRoot, Duration.Zero).contains(b.guid))
//
//    Await.result(resB.newParent.unlock(), Duration.Zero)
//
//    val res = Await.result(childOfCloneA.tryLock(), Duration.Zero)
//    assert(res === TryLockResult(success = true, b))
//    assert((res.newParent eq b) === true)
//  }
//}
