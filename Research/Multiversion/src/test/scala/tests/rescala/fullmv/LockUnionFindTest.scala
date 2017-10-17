//package tests.rescala.fullmv
//
//import org.scalatest.FunSuite
//import rescala.fullmv.mirrors.SubsumableLockHostImpl
//import rescala.fullmv.sgt.synchronization.SubsumableLock.TryLockResult
//import rescala.fullmv.sgt.synchronization.{SubsumableLockEntryPoint, SubsumableLockImpl}
//import rescala.testhelper.Spawn
//
//import scala.concurrent.duration.Duration
//import scala.concurrent.{Await, TimeoutException}
//import scala.util.{Failure, Try}
//
//class LockUnionFindTest extends FunSuite {
//  object host extends SubsumableLockHostImpl
//  test("lock works") {
//    // we can lock
//    val a = host.newLock()
//    val ae = new SubsumableLockEntryPoint(Duration.Zero, a)
//    assert(Spawn{ ae.lock() }.join(101) === a)
//
//    // lock is exclusive and blocks
//    val blockedB = Spawn{ ae.lock() }
//    intercept[TimeoutException] { blockedB.join(103) }
//
//    // unlock unblocks
//    ae.unlock()
//    assert(blockedB.join(104) === a)
//  }
//
//  test("union works") {
//    val a = host.newLock()
//    val ae = new SubsumableLockEntryPoint(Duration.Zero, a)
//    val b = host.newLock()
//    val be = new SubsumableLockEntryPoint(Duration.Zero, b)
//
//    assert(Spawn{ be.lock() }.join(101) === b)
//
//    assert(ae.trySubsume(b) === true)
//
//    assert(ae.getLockedRoot === Some(b.guid))
//    assert(be.getLockedRoot === Some(b.guid))
//
//    be.unlock()
//
//    assert(Spawn{ ae.lock() }.join(102) === a)
//    val blockedA = Spawn{ ae.lock(); ae.unlock() }
//    intercept[TimeoutException] { blockedA.join(103) }
//    val blockedB = Spawn{ be.lock(); be.unlock() }
//    intercept[TimeoutException] { blockedB.join(104) }
//
//    ae.unlock()
//
//    blockedA.join(105)
//    blockedB.join(106)
//  }
//
//  test("subsume correctly wakes all threads") {
//    val a, b = host.newLock()
//    assert(Await.result(a.tryLock(), Duration.Zero).success)
//    val resB = Await.result(b.tryLock(), Duration.Zero)
//    assert(resB.success)
//
//    var counter = 0
//    def spawnIncrementUnderLockThread(lock: SubsumableLockImpl) = {
//      Spawn {
//        val newParent = Await.result(lock.lock(), Duration.Zero)
//        val c = counter
//        counter += 1
//        Await.result(newParent.unlock(), Duration.Zero)
//        c -> newParent.guid
//      }
//    }
//
//    val queued = List.fill(5){ spawnIncrementUnderLockThread(a) } ++ List.fill(5){ spawnIncrementUnderLockThread(b) }
//
//    val timeout = System.currentTimeMillis() + 50
//    val timeouts = queued.map { thread => Try { thread.join(timeout - System.currentTimeMillis()) } }
//    assert(!timeouts.exists{
//      case Failure(_: TimeoutException) => false
//      case _ => true
//    }, s"All threads should have timed out, but some did not.")
//
//    Await.result(a.subsume(resB.newParent), Duration.Zero)
//    Await.result(resB.newParent.unlock(), Duration.Zero)
//
//    assert(queued.map(_.join(50)).toSet === (0 until 10).map(_ -> resB.newParent.guid).toSet)
//  }
//}
