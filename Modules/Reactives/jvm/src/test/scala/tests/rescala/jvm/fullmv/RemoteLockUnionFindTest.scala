//package tests.rescala.fullmv
//
//import org.scalatest.funsuite.AnyFunSuite
//import rescala.fullmv.sgt.synchronization.SubsumableLock.TryLockResult
//import rescala.fullmv.sgt.synchronization.{SubsumableLock, SubsumableLockImpl}
//import rescala.fullmv.transmitter.SubsumableLockTransmittable
//import rescala.testhelper.Spawn
//import retier.communicator.tcp.TCP
//import retier.registry.{Binding, BindingBuilder, Registry}
//import retier.serializer.upickle._
//import retier.transmitter.{Marshallable, Serializable, Transmittable}
//
//import scala.concurrent.duration._
//import scala.concurrent.{Await, TimeoutException}
//import scala.util.{Failure, Try}
//
//class RemoteLockUnionFindTest extends AnyFunSuite {
//  test("remote transfer works") {
////    val serializable = implicitly[Serializable[(Unit, Nothing, Long, (Int, TryLockResult), TryLockResult)]]
//    val serializable = upickleBasedSerializable[SubsumableLockTransmittable.Message](
//      implicitly[upickle.default.Reader[(Unit, Nothing, Long, (Int, Boolean, SubsumableLock, Long), (Boolean, SubsumableLock, Long))]],
//      implicitly[upickle.default.Writer[(Unit, Nothing, Long, (Int, Boolean, SubsumableLock, Long), (Boolean, SubsumableLock, Long))]])
//    val transmittable = SubsumableLockTransmittable.subsumableLockTransmittable(Transmittable.identical, serializable)
//    val binding = Binding[SubsumableLock]("lock")(BindingBuilder.value(Marshallable.marshallable(transmittable, serializable)))
//
//    val onHost = new SubsumableLockImpl()
//    val host = new Registry()
//    try {
//      host.listen(TCP(1099))
//      host.bind(binding)(onHost)
//
//      val client = new Registry()
//      try {
//        val onClient = Await result(client.request(TCP("localhost", 1099)), 1.second)
//        assert(onClient != onHost)
//      } finally {
//        client.terminate()
//      }
//    } finally {
//      host.terminate()
//    }
//  }
//
//  test("tryLock works"){
//    // we can lock
//    val a = new SubsumableLockImpl()
//    assertEquals(a.tryLock(), TryLockResult(success = true, a, a.gUID))
//
//    // lock is exclusive
//    assertEquals(a.tryLock(), TryLockResult(success = false, a, a.gUID))
//
//    // unlock works
//    a.unlock()
//    assertEquals(a.tryLock(), TryLockResult(success = true, a, a.gUID))
//  }
//
//  test("lock works") {
//    // we can lock
//    val a = new SubsumableLockImpl()
//    assertEquals(Spawn{a.lock()}.join(101), TryLockResult(success = true, a, a.gUID))
//
//    // lock is exclusive
//    assertEquals(a.tryLock(), TryLockResult(success = false, a, a.gUID))
//    // and blocks..
//    val blockedB = Spawn{a.lock()}
//    intercept[TimeoutException] {
//      blockedB.join(103)
//    }
//
//    // unlock unblocks
//    a.unlock()
//    assertEquals(blockedB.join(104), TryLockResult(success = true, a, a.gUID))
//  }
//
//  test("union works") {
//    val a = new SubsumableLockImpl
//    val b = new SubsumableLockImpl
//
//    assertEquals(a.tryLock(), TryLockResult(success = true, a, a.gUID))
//    val resB = b.tryLock()
//    assertEquals(resB, TryLockResult(success = true, b, b.gUID))
//
//    a.subsume(resB)
//
//    assert(a.getLockedRoot.contains(b.gUID))
//    assert(b.getLockedRoot.contains(b.gUID))
//
//    b.unlock()
//
//    assertEquals(a.tryLock(), TryLockResult(success = true, b, b.gUID))
//    assertEquals(a.tryLock(), TryLockResult(success = false, b, b.gUID))
//    assertEquals(b.tryLock(), TryLockResult(success = false, b, b.gUID))
//
//    b.unlock()
//
//    assertEquals(b.tryLock(), TryLockResult(success = true, b, b.gUID))
//    assertEquals(b.tryLock(), TryLockResult(success = false, b, b.gUID))
//    assertEquals(a.tryLock(), TryLockResult(success = false, b, b.gUID))
//  }
//
//  test("subsume correctly wakes all threads") {
//    val a, b = new SubsumableLockImpl()
//    assert(a.tryLock().success)
//    val resB = b.tryLock()
//    assert(resB.success)
//
//    var counter = 0
//    def spawnIncrementUnderLockThread(lock: SubsumableLockImpl) = {
//      Spawn {
//        val res = lock.lock()
//        assert(res.success)
//        val c = counter
//        counter += 1
//        res.newParent.unlock()
//        c -> res.globalRoot
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
//    a.subsume(resB)
//    resB.newParent.unlock()
//
//    assertEquals(queued.map(_.join(50)).toSet, (0 until 10).map(_ -> resB.globalRoot).toSet)
//  }
//}
