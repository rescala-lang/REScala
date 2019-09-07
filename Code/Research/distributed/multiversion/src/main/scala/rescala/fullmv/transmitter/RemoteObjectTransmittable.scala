package rescala.fullmv.transmitter

import java.util.concurrent.{ConcurrentHashMap, ThreadLocalRandom}
import java.util.concurrent.locks.LockSupport

import loci.transmitter._

import scala.annotation.tailrec

object RemoteObjectTransmittable{
  type Type[MirrorInitialization, HomeToMirrorUpdate, MirrorToHomeRequest, HomeToMirrorResponse] = (MirrorInitialization, HomeToMirrorUpdate, Long, MirrorToHomeRequest, HomeToMirrorResponse)
}

abstract class RemoteObjectTransmittable[Home, Mirror, MirrorInitialization, HomeToMirrorUpdate, MirrorToHomeRequest, HomeToMirrorResponse, S](implicit
                                                                                               messageTransmittable: Transmittable[
                                                                                                 (MirrorInitialization, HomeToMirrorUpdate, Long, MirrorToHomeRequest, HomeToMirrorResponse),
                                                                                                 S,
                                                                                                 (MirrorInitialization, HomeToMirrorUpdate, Long, MirrorToHomeRequest, HomeToMirrorResponse)],
                                                                                               serializable: Serializable[S]) extends PushBasedTransmittable[
                                                                                                 Home,
                                                                                                 (MirrorInitialization, HomeToMirrorUpdate, Long, MirrorToHomeRequest, HomeToMirrorResponse),
                                                                                                 S,
                                                                                                 (MirrorInitialization, HomeToMirrorUpdate, Long, MirrorToHomeRequest, HomeToMirrorResponse),
                                                                                                 Mirror] {
  type Message = (MirrorInitialization, HomeToMirrorUpdate, Long, MirrorToHomeRequest, HomeToMirrorResponse)

  var requestTracker = new ConcurrentHashMap[Long, Any]()

  override def send(value: Home, remote: RemoteRef, endpoint: Endpoint[Message, Message]): Message = {
    endpoint.receive.notify( msg =>
      endpoint.send((null.asInstanceOf[MirrorInitialization], null.asInstanceOf[HomeToMirrorUpdate], msg._3, null.asInstanceOf[MirrorToHomeRequest], dispatch(value, msg._4)))
    )
    val initializationValue = createInitializationAndConnectUpdates(value, { update =>
      endpoint.send((null.asInstanceOf[MirrorInitialization], update, 0, null.asInstanceOf[MirrorToHomeRequest], null.asInstanceOf[HomeToMirrorResponse]))
    })
    (initializationValue, null.asInstanceOf[HomeToMirrorUpdate], 0, null.asInstanceOf[MirrorToHomeRequest], null.asInstanceOf[HomeToMirrorResponse])
  }

  def dispatch(localInstance: Home, request: MirrorToHomeRequest): HomeToMirrorResponse
  def createInitializationAndConnectUpdates(value: Home, update: HomeToMirrorUpdate => Unit): MirrorInitialization

  override def receive(value: Message, remote: RemoteRef, endpoint: Endpoint[Message, Message]): Mirror = {
    val mirror = instantiateMirror(value._1, { request: MirrorToHomeRequest =>
      @tailrec def createRequest(): Long = {
        val requestId = ThreadLocalRandom.current().nextLong()
        val previousIdOwner = requestTracker.putIfAbsent(requestId, Thread.currentThread())
        if(previousIdOwner == null) requestId else createRequest()
      }
      val requestId = createRequest()
      endpoint.send((null.asInstanceOf[MirrorInitialization], null.asInstanceOf[HomeToMirrorUpdate], requestId, request, null.asInstanceOf[HomeToMirrorResponse]))
      LockSupport.park(this)
      requestTracker.remove(requestId).asInstanceOf[HomeToMirrorResponse]
    })

    endpoint.receive.notify[Message] {
      case (null, null, requestId, null, response) =>
        val thread = requestTracker.put(requestId, response).asInstanceOf[Thread]
        assert(thread != null)
        LockSupport.unpark(thread)
      case (null, update, 0, null, null) =>
        updateMirror(mirror, update)
      case otherwise =>
        throw new ClassCastException("Not a HomeToMirrorResponse or HomeToMirrorUpdate: " + otherwise)
    }

    mirror
  }

  def instantiateMirror(value: MirrorInitialization, request: MirrorToHomeRequest => HomeToMirrorResponse): Mirror
  def updateMirror(mirror: Mirror, update: HomeToMirrorUpdate): Unit
}
