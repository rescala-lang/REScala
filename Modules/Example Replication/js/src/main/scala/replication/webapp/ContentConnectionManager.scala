package replication.webapp

import com.github.plokhotnyuk.jsoniter_scala.core.*
import loci.communicator.ws.webnative.WS
import loci.registry.Registry
import loci.transmitter.RemoteRef
import org.scalajs.dom
import rescala.default.*
import rescala.structures.RExceptions.EmptySignalControlThrowable

import java.util.NoSuchElementException
import scala.concurrent.Future
import scala.scalajs.js
import scala.scalajs.js.typedarray.{Int8Array, TypedArrayBuffer}
import scala.scalajs.concurrent.JSExecutionContext.Implicits.queue

class ContentConnectionManager(registry: Registry) {

  val wsUri: String = {
    val wsProtocol = if (dom.document.location.protocol == "https:") "wss" else "ws"
    val path       = dom.document.location.pathname
    val li         = path.lastIndexOf('/')
    val parent =
      if li < 0
      then "/"
      else path.substring(0, li + 1)
    s"$wsProtocol://${dom.document.location.host}${parent}ws"
  }

  val joined = Event.fromCallback {
    registry.remoteJoined.foreach(Event.handle)
  }.event
  val left = Event.fromCallback {
    registry.remoteLeft.foreach(Event.handle)
  }.event

  val connectedRemotes = Fold(Map.empty[RemoteRef, Boolean])(
    joined act { rr => current.updated(rr, true) },
    left act { rr => current.updated(rr, false) }
  )

  val connectionStatusChanged = joined || left

  val _connectionAttempt = Var.empty[Signal[RemoteRef]]
  val connectionAttempt  = _connectionAttempt.flatten

  def connect(): Unit = {
    _connectionAttempt.set(Signal.fromFuture(tryConnect()))
  }

  def tryConnect(): Future[RemoteRef] = {
    println(s"trying to connect to $wsUri")
    registry.connect(WS(wsUri))
  }
}
