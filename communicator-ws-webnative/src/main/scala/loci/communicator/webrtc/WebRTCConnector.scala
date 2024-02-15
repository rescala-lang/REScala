package loci
package communicator
package webrtc

import channel.MesageBufferExtensions.asArrayBuffer
import channel.{InChan, JsArrayBufferMessageBuffer, MessageBuffer, OutChan, Prod}
import de.rmgk.delay
import de.rmgk.delay.{Async, Sync, syntax}
import org.scalajs.dom
import org.scalajs.dom.{RTCIceConnectionState, RTCIceGatheringState, RTCSessionDescription}

import scala.annotation.nowarn
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.concurrent.duration.*
import scala.scalajs.js
import scala.scalajs.js.JSConverters.*
import scala.scalajs.js.typedarray.ArrayBuffer
import scala.util.control.NonFatal
import scala.util.{Failure, Success, Try}

private object WebRTCConnector {
  // label seems mostly for auto negotiation
  val channelLabel = "loci-webrtc-channel"
  // id is used for pre negotiated channels
  val channelId: Double = 4
}


class WebRTCConnector(configuration: dom.RTCConfiguration) {

  println(s"starting new peer connection")
  val peerConnection = new dom.RTCPeerConnection(configuration)

  def offer(options: dom.RTCOfferOptions): Async[Any, SessionDescription] = Async {
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
    peerConnection.asInstanceOf[js.Dynamic].setLocalDescription().asInstanceOf[
      js.Promise[RTCSessionDescription]
    ].toFuture.toAsync.bind

    println(s"awaiting ice candidates ...")

    Async.fromCallback {
      peerConnection.onicecandidate = { (event: dom.RTCPeerConnectionIceEvent) =>
        if event.candidate != null then
          // we could do incremental ice, here, but seems fast enough, so eh
          ()
        else // I guess this means we are done?
          println(s"ice gathering done")
          Async.handler.succeed(SessionDescription(peerConnection.localDescription))
      }
      if peerConnection.iceGatheringState == RTCIceGatheringState.complete
      then
        println(s"ice gathering pre-done")
        Async.handler.succeed(SessionDescription(peerConnection.localDescription))

    }.bind
  }
}
