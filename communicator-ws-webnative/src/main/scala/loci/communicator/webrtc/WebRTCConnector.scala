package loci
package communicator
package webrtc

import channel.MesageBufferExtensions.asArrayBuffer
import channel.{InChan, JsArrayBufferMessageBuffer, MessageBuffer, OutChan, Prod}
import de.rmgk.delay
import de.rmgk.delay.{Async, Sync, syntax}
import org.scalajs.dom
import org.scalajs.dom.{
  RTCIceCandidate, RTCIceConnectionState, RTCIceGatheringState, RTCSessionDescription, RTCSignalingState
}

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

  def accept(session: SessionDescription): Async[Any, SessionDescription] = Async {
    println(s"setting complete session")
    peerConnection.setRemoteDescription(session.sessionDescription).toFuture.toAsync.bind
    smartLocalDescription.bind
  }

  def restartIce(): Unit =
    peerConnection.asInstanceOf[js.Dynamic].restartIce()
    ()

  def smartLocalDescription: Async[Any, SessionDescription] = Async {
    // the `setLocalDescription()` seems to be a newer API that does not yet exists in the scala wrapper :(
    // it should automagically figure out the local description we should have in the current state
    val res = peerConnection.asInstanceOf[js.Dynamic].setLocalDescription().asInstanceOf[
      js.Promise[RTCSessionDescription]
    ].toFuture.toAsync.bind
    SessionDescription(peerConnection.localDescription).get
  }

  def iceCandidates: Async[Any, RTCIceCandidate] = Async.fromCallback:
    peerConnection.addEventListener(
      "icecandidate",
      (event: dom.RTCPeerConnectionIceEvent) =>
        if !js.isUndefined(event.candidate) && event.candidate != null && event.candidate.candidate != "" then
          Async.handler.succeed(event.candidate)
    )

  def readLifecycle =
    ConnectorLifecycle(
      SessionDescription(peerConnection.localDescription),
      SessionDescription(peerConnection.remoteDescription),
      peerConnection.iceGatheringState,
      peerConnection.iceConnectionState,
      peerConnection.signalingState
    )

  /** For reasons that are unclear to me, a data channel MUST be created before looking at the lifecycle, otherwise ICE gathering does not happen */
  def lifecycle: Async[Any, ConnectorLifecycle] = Async {

    Async.fromCallback {
      peerConnection.addEventListener(
        "icecandidate",
        (event: dom.RTCPeerConnectionIceEvent) =>
          println(s"lifecycle ice tricke")
          if event.candidate != null then
            // we could do incremental ice, here, but seems fast enough, so eh
            println(s"incremental ice ${event.candidate}")
            Async.handler.succeed(())
          else // I guess this means we are done?
            println(s"ice gathering done")
            Async.handler.succeed(())
      )

      peerConnection.oniceconnectionstatechange = (_: dom.Event) =>
        Async.handler.succeed(())

      peerConnection.onsignalingstatechange = (_: dom.Event) =>
        Async.handler.succeed(())

      peerConnection.onnegotiationneeded = (_: dom.Event) =>
        Async.handler.succeed(())

      // directly succeed to produce a first value
      Async.handler.succeed(())
    }.bind

    readLifecycle
  }
}
