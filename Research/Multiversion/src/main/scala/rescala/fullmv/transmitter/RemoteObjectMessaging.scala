package rescala.fullmv.transmitter

import java.util.concurrent.{ConcurrentHashMap, ThreadLocalRandom}
import java.util.concurrent.locks.LockSupport

import retier.transmitter._

import scala.annotation.tailrec

sealed trait RemoteObjectMessaging[+MirrorInitialization, +HomeToMirrorUpdate, +MirrorToHomeRequest, +HomeToMirrorResponse]
object RemoteObjectMessaging{
  case class MirrorInitialization[T](initialTransfer: T) extends RemoteObjectMessaging[T, Nothing, Nothing, Nothing]
  case class HomeToMirrorUpdate[T](update: T) extends RemoteObjectMessaging[Nothing, T, Nothing, Nothing]
  case class MirrorToHomeRequest[T](requestId: Long, request: T) extends RemoteObjectMessaging[Nothing, Nothing, T, Nothing] {
    def map[R](f: T => R) = HomeToMirrorResponse(requestId, f(request))
  }
  case class HomeToMirrorResponse[T](requestId: Long, response: T) extends RemoteObjectMessaging[Nothing, Nothing, Nothing, T]
}

abstract class RemoteObjectTransmittable[Home, Mirror, MirrorInitialization, HomeToMirrorUpdate, MirrorToHomeRequest, HomeToMirrorResponse, S](implicit
                                                                                               messageTransmittable: Transmittable[
                                                                                                 RemoteObjectMessaging[MirrorInitialization, HomeToMirrorUpdate, MirrorToHomeRequest, HomeToMirrorResponse],
                                                                                                 S,
                                                                                                 RemoteObjectMessaging[MirrorInitialization, HomeToMirrorUpdate, MirrorToHomeRequest, HomeToMirrorResponse]],
                                                                                               serializable: Serializable[S]) extends PushBasedTransmittable[
                                                                                                 Home,
                                                                                                 RemoteObjectMessaging[MirrorInitialization, HomeToMirrorUpdate, MirrorToHomeRequest, HomeToMirrorResponse],
                                                                                                 S,
                                                                                                 RemoteObjectMessaging[MirrorInitialization, HomeToMirrorUpdate, MirrorToHomeRequest, HomeToMirrorResponse],
                                                                                                 Mirror] {
  type Message = RemoteObjectMessaging[MirrorInitialization, HomeToMirrorUpdate, MirrorToHomeRequest, HomeToMirrorResponse]

  var requestTracker = new ConcurrentHashMap[Long, Any]()

  override def send(value: Home, remote: RemoteRef, endpoint: Endpoint[Message, Message]): Message = {
    endpoint.receive.notify( msg =>
      endpoint.send(msg.asInstanceOf[RemoteObjectMessaging.MirrorToHomeRequest[MirrorToHomeRequest]].map(dispatch(value, _)))
    )
    RemoteObjectMessaging.MirrorInitialization(createInitializationAndConnectUpdates(value, { update =>
      endpoint.send(RemoteObjectMessaging.HomeToMirrorUpdate(update))
    }))
  }

  def dispatch(localInstance: Home, request: MirrorToHomeRequest): HomeToMirrorResponse
  def createInitializationAndConnectUpdates(value: Home, update: HomeToMirrorUpdate => Unit): MirrorInitialization

  override def receive(value: Message, remote: RemoteRef, endpoint: Endpoint[Message, Message]): Mirror = {
    val mirror = instantiateMirror(value.asInstanceOf[RemoteObjectMessaging.MirrorInitialization[MirrorInitialization]].initialTransfer, { request: MirrorToHomeRequest =>
      @tailrec def createRequest(): Long = {
        val requestId = ThreadLocalRandom.current().nextLong()
        val previousIdOwner = requestTracker.putIfAbsent(requestId, Thread.currentThread())
        if(previousIdOwner == null) requestId else createRequest()
      }
      val requestId = createRequest()
      endpoint.send(RemoteObjectMessaging.MirrorToHomeRequest(requestId, request))
      LockSupport.park(this)
      requestTracker.remove(requestId).asInstanceOf[HomeToMirrorResponse]
    })

    endpoint.receive.notify[Message] {
      case RemoteObjectMessaging.HomeToMirrorResponse(requestId, response) =>
        val thread = requestTracker.put(requestId, response).asInstanceOf[Thread]
        assert(thread != null)
        LockSupport.unpark(thread)
      case RemoteObjectMessaging.HomeToMirrorUpdate(update) =>
        updateMirror(mirror, update)
      case neither =>
        throw new ClassCastException("Not a HomeToMirrorResponse or HomeToMirrorUpdate: " + neither)
    }

    mirror
  }

  def instantiateMirror(value: MirrorInitialization, request: MirrorToHomeRequest => HomeToMirrorResponse): Mirror
  def updateMirror(mirror: Mirror, update: HomeToMirrorUpdate): Unit
}
