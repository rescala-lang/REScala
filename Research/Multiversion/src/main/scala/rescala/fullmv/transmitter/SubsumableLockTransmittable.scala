package rescala.fullmv.transmitter

import java.util.concurrent.locks.LockSupport
import java.util.concurrent.{ConcurrentHashMap, ConcurrentMap, ThreadLocalRandom}

import rescala.fullmv.mirrors._
import rescala.fullmv.mirrors.Host.GUID
import rescala.fullmv.sgt.synchronization.SubsumableLock
import rescala.fullmv.sgt.synchronization.SubsumableLock.TryLockResult
import retier.transmitter._

import scala.annotation.tailrec

object SubsumableLockTransmittable {
  type EndPointWithInfrastructure[T] = Endpoint[MessageWithInfrastructure[T], MessageWithInfrastructure[T]]
  type MessageWithInfrastructure[T] = (Host.GUID, Int, Long, T)
  type ParametersOrReturns = (Long, Boolean, Host.GUID, Host.GUID)

  def subsumableLockTransmittable[S](host: SubsumableLockHost)(implicit messageTransmittable: Transmittable[MessageWithInfrastructure[ParametersOrReturns], S, MessageWithInfrastructure[ParametersOrReturns]], serializable: Serializable[S]): Transmittable[SubsumableLock, S, SubsumableLock] = new PushBasedTransmittable[SubsumableLock, MessageWithInfrastructure[ParametersOrReturns], S, MessageWithInfrastructure[ParametersOrReturns], SubsumableLock] {
    val receiverCache = new ConcurrentHashMap[Host.GUID, SubsumableLockMirrorProxy]()
    val requestTracker = new ConcurrentHashMap[Long, Any]()

    def handleRequest(endpoint: EndPointWithInfrastructure[ParametersOrReturns], receiver: SubsumableLockMirrorProxy, op: Int, parameters: ParametersOrReturns): ParametersOrReturns = (op, parameters) match {
      case (0, (_, success, newParent, root)) =>
        receiver.subsume(TryLockResult(success, new SubsumableLockReflection(host, newParent, new SubsumableLockMirrorProxyToEndpoint(doRequest(endpoint, newParent), receiverCache)), root))
        (0L, false, Host.dummyGuid, Host.dummyGuid)
      case (1, (_, _, _, _)) =>
        receiver.unlock()
        (0L, false, Host.dummyGuid, Host.dummyGuid)
      case (2, (_, _, _, _)) =>
        val res = receiver.getLockedRoot
        (0L, res.isDefined, Host.dummyGuid, res.getOrElse(Host.dummyGuid))
      case (3, (_, _, _, _)) =>
        val (success, root) = receiver.tryLock()
        (0L, success, Host.dummyGuid, root)
      case (4, (_, _, _, _)) =>
        val root = receiver.lock()
        (0L, false, Host.dummyGuid, root)
      case (5, (backoff, _, _, _)) =>
        val root = receiver.spinOnce(backoff)
        (0L, false, Host.dummyGuid, root)
      case (6, (_, success, newParent, root)) =>
        val res = receiver.trySubsume(TryLockResult(success, new SubsumableLockReflection(host, newParent, new SubsumableLockMirrorProxyToEndpoint(doRequest(endpoint, newParent), receiverCache)), root))
        (0L, res, Host.dummyGuid, Host.dummyGuid)
      case otherwise =>
        throw new AssertionError("undefined message : "+otherwise)
    }

    def doRequest(endpoint: EndPointWithInfrastructure[ParametersOrReturns], receiver: Host.GUID)(op: Int, parameters: ParametersOrReturns): ParametersOrReturns = {
      @tailrec def createRequest(): Long = {
        val requestId = ThreadLocalRandom.current().nextLong()
        val previousIdOwner = requestTracker.putIfAbsent(requestId, Thread.currentThread())
        if(previousIdOwner == null) requestId else createRequest()
      }
      val requestId = createRequest()
      endpoint.send((receiver, op, requestId, parameters))
      LockSupport.park(this)
      requestTracker.remove(requestId).asInstanceOf[(Long, Boolean, GUID, GUID)]
    }

    def handleResponse(requestId: Long, returns: ParametersOrReturns): Unit = {
      val thread = requestTracker.put(requestId, returns).asInstanceOf[Thread]
      assert(thread != null)
      LockSupport.unpark(thread)
    }

    def handleMessage(endpoint: EndPointWithInfrastructure[ParametersOrReturns])(msg: MessageWithInfrastructure[ParametersOrReturns]): Unit = msg match {
      case (Host.dummyGuid, _, requestId, returns) =>
        handleResponse(requestId, returns)
      case (receiver, op, requestId, parameters) =>
        val response = handleRequest(endpoint, receiverCache.get(receiver), op, parameters)
        endpoint.send((Host.dummyGuid, 0, requestId, response))
    }
    override def send(value: SubsumableLock, remote: RemoteRef, endpoint: EndPointWithInfrastructure[ParametersOrReturns]): MessageWithInfrastructure[ParametersOrReturns] = {
      assert(value.host == host)
      receiverCache.putIfAbsent(value.guid, new SubsumableLockMirror(value))
      endpoint.receive.notify (handleMessage(endpoint))
      (value.guid, 0, 0L, (0L, false, Host.dummyGuid, Host.dummyGuid))
    }

    override def receive(value: MessageWithInfrastructure[ParametersOrReturns], remote: RemoteRef, endpoint: EndPointWithInfrastructure[ParametersOrReturns]): SubsumableLock = {
      endpoint.receive.notify (handleMessage(endpoint))
      new SubsumableLockReflection(host, value._1, new SubsumableLockMirrorProxyToEndpoint(doRequest(endpoint, value._1), receiverCache))
    }
  }
}

class SubsumableLockMirrorProxyToEndpoint(doRequest: (Int, (Long, Boolean, GUID, GUID)) => (Long, Boolean, GUID, GUID), receiverCache: ConcurrentMap[Host.GUID, SubsumableLockMirrorProxy]) extends SubsumableLockMirrorProxy {
  override def subsume(lockedNewParent: TryLockResult): Unit = {
    receiverCache.putIfAbsent(lockedNewParent.newParent.guid, new SubsumableLockMirror(lockedNewParent.newParent))
    doRequest(0, (0L, lockedNewParent.success, lockedNewParent.newParent.guid, lockedNewParent.globalRoot))
  }
  override def unlock(): Unit = {
    doRequest(1, (0L, false, Host.dummyGuid, Host.dummyGuid))
  }
  override def getLockedRoot: Option[GUID] = {
    val (_, success, _, root) = doRequest(2, (0L, false, Host.dummyGuid, Host.dummyGuid))
    if(success) Some(root) else None
  }
  override def tryLock(): (Boolean, GUID) = {
    val (_, success, _, root) = doRequest(3, (0L, false, Host.dummyGuid, Host.dummyGuid))
    (success, root)
  }
  override def lock(): GUID = {
    val (_, _, _, root) = doRequest(4, (0L, false, Host.dummyGuid, Host.dummyGuid))
    root
  }
  override def spinOnce(backoff: GUID): GUID = {
    val (_, _, _, root) = doRequest(5, (backoff, false, Host.dummyGuid, Host.dummyGuid))
    root
  }
  override def trySubsume(lockedNewParent: TryLockResult): Boolean = {
    val added = receiverCache.putIfAbsent(lockedNewParent.newParent.guid, new SubsumableLockMirror(lockedNewParent.newParent)) == null
    val (_, success, _, _) = doRequest(6, (0L, lockedNewParent.success, lockedNewParent.newParent.guid, lockedNewParent.globalRoot))
    if(added && !success) {
      val removed = receiverCache.remove(lockedNewParent.newParent.guid, lockedNewParent.newParent)
      assert(removed)
    }
    // TODO this may be safe because lockedNewParent.newParent is locked and thus cannot be used by other threads concurrently, but might not be?
    success
  }
}
