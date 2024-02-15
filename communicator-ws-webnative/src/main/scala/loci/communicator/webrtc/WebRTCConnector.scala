package loci
package communicator
package webrtc

import channel.MesageBufferExtensions.asArrayBuffer
import channel.{InChan, JsArrayBufferMessageBuffer, MessageBuffer, OutChan, Prod}
import de.rmgk.delay
import de.rmgk.delay.{Async, Sync, syntax}
import org.scalajs.dom
import org.scalajs.dom.{RTCIceConnectionState, RTCIceGatheringState, RTCSessionDescription, RTCSignalingState}

import scala.annotation.nowarn
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.concurrent.duration.*
import scala.scalajs.js
import scala.scalajs.js.JSConverters.*
import scala.scalajs.js.typedarray.ArrayBuffer
import scala.util.control.NonFatal
import scala.util.{Failure, Success, Try}

case class SessionDescription(descType: String, sdp: String) {
  def sessionDescription = new dom.RTCSessionDescription(
    new dom.RTCSessionDescriptionInit {
      this.`type` = SessionDescription.this.descType.asInstanceOf[dom.RTCSdpType]
      this.sdp = SessionDescription.this.sdp
    }
  )
}

object SessionDescription {
  def apply(value: dom.RTCSessionDescription): Option[SessionDescription] =
    Option(value).map: value =>
      SessionDescription(value.`type`.asInstanceOf[String], value.sdp)
}

case class ConnectorLifecycle(
    localSession: Option[SessionDescription],
    remoteSession: Option[SessionDescription],
    iceGatheringState: RTCIceGatheringState,
    iceConnectionState: RTCIceConnectionState,
    signalingState: RTCSignalingState,
)

object WebRTCConnector {
  // label seems mostly for auto negotiation
  val channelLabel = "loci-webrtc-channel"
  // id is used for pre negotiated channels
  val channelId: Double = 4
}

class WebRTCConnector(configuration: dom.RTCConfiguration) {

  println(s"starting new peer connection")
  val peerConnection = new dom.RTCPeerConnection(configuration)

  def offer(): Async[Any, SessionDescription] = Async {
    println(s"creating offer")
    smartLocalDescription.bind
  }

  private var remoteDescriptionSet = false

  def accept(session: SessionDescription): Async[Any, SessionDescription] = Async {
    println(s"setting complete session")
    (if !remoteDescriptionSet then
       remoteDescriptionSet = true
       peerConnection.setRemoteDescription(session.sessionDescription).toFuture
     else
       Future.successful(())
    )
    .toAsync.bind
    smartLocalDescription.bind
  }

  def smartLocalDescription: Async[Any, SessionDescription] = Async {
    // the `setLocalDescription()` seems to be a newer API that does not yet exists in the scala wrapper :(
    // it should automagically figure out the local description we should have in the current state
    val res = peerConnection.asInstanceOf[js.Dynamic].setLocalDescription().asInstanceOf[
      js.Promise[RTCSessionDescription]
    ].toFuture.toAsync.bind
    SessionDescription(peerConnection.localDescription).get
  }

  def readLifecycle =
    ConnectorLifecycle(
      SessionDescription(peerConnection.localDescription),
      SessionDescription(peerConnection.remoteDescription),
      peerConnection.iceGatheringState,
      peerConnection.iceConnectionState,
      peerConnection.signalingState
    )

  def lifecycle: Async[Any, ConnectorLifecycle] = Async {

    Async.fromCallback {
      peerConnection.onicecandidate = (event: dom.RTCPeerConnectionIceEvent) =>
        if event.candidate != null then
          // we could do incremental ice, here, but seems fast enough, so eh
          ()
        else // I guess this means we are done?
          println(s"ice gathering done")
          Async.handler.succeed(())

      peerConnection.oniceconnectionstatechange = (_: dom.Event) =>
        Async.handler.succeed(())

      peerConnection.onsignalingstatechange = (_: dom.Event) =>
        Async.handler.succeed(())

      // directly succeed to produce a first value
      Async.handler.succeed(())
    }.bind

    readLifecycle
  }
}
