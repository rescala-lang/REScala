package loci
package communicator
package webrtc

import channel.MesageBufferExtensions.asArrayBuffer
import channel.{InChan, JsArrayBufferMessageBuffer, MessageBuffer, OutChan, Prod}
import de.rmgk.delay
import de.rmgk.delay.{Async, Sync}
import org.scalajs.dom

import scala.annotation.nowarn
import scala.concurrent.duration.*
import scala.scalajs.js
import scala.scalajs.js.typedarray.ArrayBuffer
import scala.util.{Failure, Success, Try}
import scala.util.control.NonFatal

private object WebRTCConnector {
  val channelLabel = "loci-webrtc-channel"
}

class WebRTCConnector(configuration: dom.RTCConfiguration) {

  println(s"starting new peer connection")
  val peerConnection = new dom.RTCPeerConnection(configuration)

  // peerConnection.setLocalDescription()

  def iceCandidates: Async[Any, CompleteSession] = Async.fromCallback {
    peerConnection.onicecandidate = { (event: dom.RTCPeerConnectionIceEvent) =>
      println(s"received ICE candidate")
      if event.candidate != null then
        println(s"uhh … unknown event candidate, dont know what to do")
      else
        Async.handler.succeed(CompleteSession(peerConnection.localDescription))
    }
  }

  def incomingDataChannel: Async[Any, WebRTCConnection] = Async.fromCallback {
    peerConnection.ondatachannel = { (event: dom.RTCDataChannelEvent) =>
      println(s"received data channel connection on ${event.channel.label}")
      if (event.channel.label == WebRTCConnector.channelLabel) {
        WebRTCConnection.open(event.channel).run(Async.handler)
      }
    }
  }

  def connect(options: dom.RTCOfferOptions): Async[Any, WebRTCConnection] = Async.fromCallback {
    println(s"trying to connect")
    try {
      // as far as I understand, this will try to autonegotiate the data the data connection … ?
      val channel = peerConnection.createDataChannel(
        WebRTCConnector.channelLabel,
        new dom.RTCDataChannelInit {}
      )

      peerConnection.createOffer(options) `then` { (description: dom.RTCSessionDescription) =>
        // todo is this needed?
        peerConnection.setLocalDescription(description) `then` { (_: Unit) =>
          println(s"applying initial session to incremental, but incremental was removed")
          unit
        }
        unit
      }

      WebRTCConnection.open(channel).run(Async.handler)

    } catch {
      case NonFatal(exception) =>
        Async.handler.fail(WebRTCConnectionFailed(s"some exception happened $exception"))
      /*connectionEstablished.set(Failure(exception))*/
    }

  }

  // todo

  private var remoteDescriptionSet = false

  def set(session: CompleteSession) =
    println(s"setting update ")
    println(s"setting complete session")
    if (!remoteDescriptionSet) {
      println(s"new remote description")
      remoteDescriptionSet = true
      setRemoteDescription(session.sessionDescription)
    }

  protected val unit = js.|.from[Unit, Unit, js.Thenable[Unit]](())

  @nowarn("msg=discarded non-Unit value")
  def setRemoteDescription(description: dom.RTCSessionDescription): Unit =
    peerConnection.setRemoteDescription(description) `then` { (_: Unit) =>
      if peerConnection.localDescription == null then
        peerConnection.createAnswer() `then` { (description: dom.RTCSessionDescription) =>
          peerConnection.setLocalDescription(description) `then` { (_: Unit) =>
            println(s"remote session description arrived, but nothing happens")
            // update.left foreach { _(InitialSession(description)) }
            unit
          }
          unit
        }
      unit
    }
}
