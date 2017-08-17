package rescala.fullmv.transmitter

import java.util.concurrent.locks.LockSupport
import java.util.concurrent.{ConcurrentHashMap, ThreadLocalRandom}

import rescala.fullmv.mirrors._
import rescala.fullmv.sgt.synchronization.SubsumableLock
import rescala.fullmv.sgt.synchronization.SubsumableLock.TryLockResult
import retier.transmitter._

import scala.annotation.tailrec
import scala.concurrent.Future

object SubsumableLockTransmittable {
  val DEBUG = false

  type EndPointWithInfrastructure[T] = Endpoint[MessageWithInfrastructure[T], MessageWithInfrastructure[T]]
  type MessageWithInfrastructure[T] = (Host.GUID, Int, Long, T)
  type ParametersOrReturns = (Long, Boolean, Host.GUID)

  def subsumableLockTransmittable[S](host: SubsumableLockHost)(implicit messageTransmittable: Transmittable[MessageWithInfrastructure[ParametersOrReturns], S, MessageWithInfrastructure[ParametersOrReturns]], serializable: Serializable[S]): Transmittable[SubsumableLock, S, SubsumableLock] = new PushBasedTransmittable[SubsumableLock, MessageWithInfrastructure[ParametersOrReturns], S, MessageWithInfrastructure[ParametersOrReturns], SubsumableLock] {
    val requestTracker = new ConcurrentHashMap[Long, Any]()

    def handleResponse(requestId: Long, returns: ParametersOrReturns): Unit = {
      val thread = requestTracker.put(requestId, returns).asInstanceOf[Thread]
      assert(thread != null, s"request $requestId: no thread registered!")
      LockSupport.unpark(thread)
    }

    def handleMessage(endpoint: EndPointWithInfrastructure[ParametersOrReturns])(msg: MessageWithInfrastructure[ParametersOrReturns]): Unit = msg match {
      case (Host.dummyGuid, _, requestId, returns) =>
        handleResponse(requestId, returns)
      case (receiver, op, requestId, parameters) =>
        // TODO better choice than global?
        import scala.concurrent.ExecutionContext.Implicits.global
        Future {
          val response = handleRequest(getCachedOrReceiveRemote(_, doRequest(endpoint)), host.getInstance(receiver), op, parameters)
          endpoint.send((Host.dummyGuid, 0, requestId, response))
        }
    }

    override def send(value: SubsumableLock, remote: RemoteRef, endpoint: EndPointWithInfrastructure[ParametersOrReturns]): MessageWithInfrastructure[ParametersOrReturns] = {
      assert(value.host == host)
      endpoint.receive.notify (handleMessage(endpoint))
      (value.guid, 0, 0L, (0L, false, Host.dummyGuid))
    }

    def doRequest(endpoint: EndPointWithInfrastructure[ParametersOrReturns])(receiver: Host.GUID, op: Int, parameters: ParametersOrReturns): ParametersOrReturns = {
      @tailrec def createRequest(): Long = {
        val requestId = ThreadLocalRandom.current().nextLong()
        val previousIdOwner = requestTracker.putIfAbsent(requestId, Thread.currentThread())
        if(previousIdOwner == null) requestId else createRequest()
      }
      val requestId = createRequest()
      if(DEBUG) println(s"[${Thread.currentThread().getName}] send request $requestId: ($op, $parameters) to $receiver")
      endpoint.send((receiver, op, requestId, parameters))
      while(requestTracker.get(requestId) == Thread.currentThread()) LockSupport.park(this)
      val res = requestTracker.remove(requestId)
      assert(res != null, s"request $requestId: unparked, but no result!")
      res.asInstanceOf[ParametersOrReturns]
    }

    def getCachedOrReceiveRemote(newParent: Host.GUID, doRequest: (Host.GUID, Int, ParametersOrReturns) => ParametersOrReturns): SubsumableLock = {
      host.getCachedOrReceiveRemote(newParent) { doCache =>
        val instance = new SubsumableLockReflection(host, newParent, new SubsumableLockMirrorProxyToEndpoint(newParent, doRequest))
        doCache(instance)
        instance
      }
    }

    override def receive(value: MessageWithInfrastructure[ParametersOrReturns], remote: RemoteRef, endpoint: EndPointWithInfrastructure[ParametersOrReturns]): SubsumableLock = {
      endpoint.receive.notify (handleMessage(endpoint))
      getCachedOrReceiveRemote(value._1, doRequest(endpoint))
    }

    def handleRequest(getCachedOrReceiveRemote: Host.GUID => SubsumableLock, receiver: SubsumableLockProxy, op: Int, parameters: ParametersOrReturns): ParametersOrReturns = (op, parameters) match {
      case (0, (_, _, newParent)) =>
        receiver.subsume(getCachedOrReceiveRemote(newParent))
        (0L, false, Host.dummyGuid)
      case (1, (_, _, _)) =>
        val newParent = receiver.unlock()
        assert(newParent.host == host)
        (0L, false, newParent.guid)
      case (2, (_, _, _)) =>
        val res = receiver.getLockedRoot
        (0L, res.isDefined, res.getOrElse(Host.dummyGuid))
      case (3, (_, _, _)) =>
        val TryLockResult(success, newParent) = receiver.tryLock()
        assert(newParent.host == host)
        (0L, success, newParent.guid)
      case (4, (_, _, _)) =>
        val newParent = receiver.lock()
        assert(newParent.host == host)
        (0L, false, newParent.guid)
      case (5, (backoff, _, _)) =>
        val newParent = receiver.spinOnce(backoff)
        assert(newParent.host == host)
        (0L, false, newParent.guid)
      case (6, (_, _, newParent)) =>
        val res = receiver.trySubsume(getCachedOrReceiveRemote(newParent))
        (0L, res.isEmpty, res match {
          case Some(resNewParent) =>
            assert(resNewParent.host == host)
            resNewParent.guid
          case None => Host.dummyGuid
        })
      case otherwise =>
        throw new AssertionError("undefined message : "+otherwise)
    }

    class SubsumableLockMirrorProxyToEndpoint(val guid: Host.GUID, val doRequest: (Host.GUID, Int, ParametersOrReturns) => ParametersOrReturns) extends SubsumableLockProxy {
      override def subsume(lockedNewParent: SubsumableLock): Unit = {
        assert(lockedNewParent.host == host)
        doRequest(guid, 0, (0L, false, lockedNewParent.guid))
      }
      override def unlock(): SubsumableLock = {
        val(_, _, newParent) = doRequest(guid, 1, (0L, false, Host.dummyGuid))
        getCachedOrReceiveRemote(newParent, doRequest)
      }
      override def getLockedRoot: Option[Host.GUID] = {
        val (_, success, root) = doRequest(guid, 2, (0L, false, Host.dummyGuid))
        if(success) Some(root) else None
      }
      override def tryLock(): TryLockResult = {
        val (_, success, newParent) = doRequest(guid, 3, (0L, false, Host.dummyGuid))
        TryLockResult(success, getCachedOrReceiveRemote(newParent, doRequest))
      }
      override def lock(): SubsumableLock = {
        val (_, _, newParent) = doRequest(guid, 4, (0L, false, Host.dummyGuid))
        getCachedOrReceiveRemote(newParent, doRequest)
      }
      override def spinOnce(backoff: Host.GUID): SubsumableLock = {
        val (_, _, newParent) = doRequest(guid, 5, (backoff, false, Host.dummyGuid))
        getCachedOrReceiveRemote(newParent, doRequest)
      }
      override def trySubsume(lockedNewParent: SubsumableLock): Option[SubsumableLock] = {
        assert(lockedNewParent.host == host)
        val (_, success, newParent) = doRequest(guid, 6, (0L, false, lockedNewParent.guid))
        if(success) None else Some(getCachedOrReceiveRemote(newParent, doRequest))
      }
    }
  }
}
