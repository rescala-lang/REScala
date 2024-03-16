package channel.webrtc

import channel.MesageBufferExtensions.asArrayBuffer
import channel.{InChan, JsArrayBufferMessageBuffer, MessageBuffer, OutChan, Prod}
import de.rmgk.delay
import de.rmgk.delay.{Async, Sync, syntax}
import org.scalajs.dom
import org.scalajs.dom.{
  RTCConfiguration, RTCIceCandidate, RTCIceConnectionState, RTCIceGatheringState, RTCSessionDescription,
  RTCSignalingState
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

case class ConnectorOverview(
    localSession: Option[SessionDescription],
    remoteSession: Option[SessionDescription],
    iceGatheringState: RTCIceGatheringState,
    iceConnectionState: RTCIceConnectionState,
    signalingState: RTCSignalingState,
)

object WebRTCConnector {
  def apply(): WebRTCConnector                                = new WebRTCConnector(new RTCConfiguration {})
  def apply(configuration: RTCConfiguration): WebRTCConnector = new WebRTCConnector(configuration)
}

/** First listen to [[lifecycle]] and optionally [[iceCandidates]].
  * Then create a channel directly on peer connection, which will trigger the connection initiation process.
  * Somehow transfer the local session description to a remote peer, and [[updateRemoteDescription]] there.
  * Accepting a remote description will update the local description,
  * which then needs to be transferred back to the original peer and [[updateLocalDescription]] there should complete the connection.
  */
class WebRTCConnector(configuration: dom.RTCConfiguration) {

  val peerConnection = new dom.RTCPeerConnection(configuration)

  peerConnection.addEventListener(
    "negotiationneeded",
    (_: dom.Event) =>
      smartUpdateLocalDescription.run(using ()) {
        case Failure(ex) =>
          throw ex
        case _ =>
      }
  )

  /** Might yell at you if called multiple times with incompatible sessions.
    * Better just throw away the whole peer connection on retry.
    */
  def updateRemoteDescription(session: SessionDescription): Async[Any, ConnectorOverview] = Async {
    peerConnection.setRemoteDescription(session.sessionDescription).toFuture.toAsync.bind
    smartUpdateLocalDescription.bind
    readOverview
  }

  /** this exists, but unclear how it could be used to restart the connection */
  def restartIce(): Unit =
    peerConnection.asInstanceOf[js.Dynamic].restartIce()
    ()

  /** This just generates a local description that automatically figures out if it should be an offer or an answer based on the current state. */
  def smartUpdateLocalDescription: Async[Any, SessionDescription] = Async {
    // the `setLocalDescription()` seems to be a newer API that does not yet exists in the scala wrapper :(
    // it should automagically figure out the local description we should have in the current state
    val res = peerConnection.asInstanceOf[js.Dynamic].setLocalDescription().asInstanceOf[
      js.Promise[RTCSessionDescription]
    ].toFuture.toAsync.bind
    SessionDescription(peerConnection.localDescription).get
  }

  /** Use [[peerConnection.addIceCandidate]] to apply on remote.
    * Once an ICE candidate exists it is also automatically included in the local description, so this is not necessary.
    */
  def iceCandidates: Async[Any, RTCIceCandidate] = Async.fromCallback:
    peerConnection.addEventListener(
      "icecandidate",
      (event: dom.RTCPeerConnectionIceEvent) =>
        if !js.isUndefined(event.candidate) && event.candidate != null && event.candidate.candidate != "" then
          Async.handler.succeed(event.candidate)
    )

  /** Mostly to wrap the session descriptions, but having the rest is also convenient */
  def readOverview =
    ConnectorOverview(
      SessionDescription(peerConnection.localDescription),
      SessionDescription(peerConnection.remoteDescription),
      peerConnection.iceGatheringState,
      peerConnection.iceConnectionState,
      peerConnection.signalingState
    )

  /** This is triggered by [[smartUpdateLocalDescription]], which in turn is automatically triggered when adding a channel.
    * For reasons that are unclear to me, a data channel MUST be created before looking at the lifecycle, otherwise ICE gathering does not happen
    */
  def lifecycle: Async[Any, ConnectorOverview] = Async {

    Async.fromCallback {
      peerConnection.addEventListener(
        "icecandidate",
        (event: dom.RTCPeerConnectionIceEvent) =>
          if event.candidate != null then
            // we could do incremental ice, here, but seems fast enough, so eh
            Async.handler.succeed(())
          else // I guess this means we are done?
            Async.handler.succeed(())
      )

      peerConnection.addEventListener(
        "negotiationneeded",
        (_: dom.Event) =>
          Async.handler.succeed(())
      )

      peerConnection.oniceconnectionstatechange = (_: dom.Event) =>
        Async.handler.succeed(())

      peerConnection.onsignalingstatechange = (_: dom.Event) =>
        Async.handler.succeed(())

      // directly succeed to produce a first value
      Async.handler.succeed(())
    }.bind

    readOverview
  }
}
