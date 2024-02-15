package loci
package communicator
package webrtc

import WebRTC.{CompleteSession, CompleteUpdate, Connector, IncrementalUpdate, InitialSession, SessionUpdate}
import channel.JsArrayBufferMessageBuffer
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

private abstract class WebRTCConnector(
    configuration: dom.RTCConfiguration,
    update: CompleteSession => Unit
) extends WebRTC.Connector {

  println(s"starting new peer connection")
  val peerConnection = new dom.RTCPeerConnection(configuration)

  peerConnection.onicecandidate = { (event: dom.RTCPeerConnectionIceEvent) =>
    println(s"received ICE candidate")
    if (event.candidate != null) then
      println(s"uhh … unknown event candidate, dont know what to do")
    else
      update(CompleteSession(peerConnection.localDescription))
  }

  private var remoteDescriptionSet = false

  def set(update: CompleteUpdate) =
    println(s"setting update ")
    update match {
      case session: CompleteSession =>
        println(s"setting complete session")
        if (!remoteDescriptionSet) {
          println(s"new remote description")
          remoteDescriptionSet = true
          setRemoteDescription(session.sessionDescription)
        }
    }

  protected val unit = js.|.from[Unit, Unit, js.Thenable[Unit]](())

  protected def setRemoteDescription(description: dom.RTCSessionDescription): Unit
}

class WebRTCOffer(
    configuration: dom.RTCConfiguration,
    options: dom.RTCOfferOptions,
    update: CompleteSession => Unit
) extends WebRTCConnector(configuration, update) {

  println(s"creating offer")

  def connect( /*connectionEstablished: Connected[WebRTC]*/ ) =
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

      new WebRTCChannelConnector(channel).connect()
//      { connection =>
//        handleConnectionClosing(connection)
//        /*connectionEstablished.trySet(connection)*/
//      }
    } catch {
      case NonFatal(exception) =>
      /*connectionEstablished.set(Failure(exception))*/
    }

  protected def setRemoteDescription(description: dom.RTCSessionDescription) =
    peerConnection.setRemoteDescription(description)
    ()
}

private class WebRTCAnswer(
    configuration: dom.RTCConfiguration,
    update: CompleteSession => Unit
) extends WebRTCConnector(configuration, update) {

  private val connectorQueue = new js.Array[WebRTCChannelConnector](0)
  private val connectedQueue = new js.Array[WebRTCChannelConnector](0)

  private def connect() =
    if (connectorQueue.length != 0 && connectedQueue.length != 0) {
      val connector = connectorQueue.shift()
      val connected = connectedQueue.shift()
      connector.connect()
//      { connection =>
//        handleConnectionClosing(connection)
//        connected.trySet(connection)
//      }
    }

//  protected def connect() = {
//    //connectedQueue.push(connectionEstablished)
//  }

  peerConnection.ondatachannel = { (event: dom.RTCDataChannelEvent) =>
    if (event.channel.label == WebRTCConnector.channelLabel) {
      connectorQueue.push(new WebRTCChannelConnector(event.channel))
      connect()
    }
  }

  @nowarn("msg=discarded non-Unit value")
  protected def setRemoteDescription(description: dom.RTCSessionDescription) =
    peerConnection.setRemoteDescription(description) `then` { (_: Unit) =>
      peerConnection.createAnswer() `then` { (description: dom.RTCSessionDescription) =>
        peerConnection.setLocalDescription(description) `then` { (_: Unit) =>
          println(s"remote session description arrived, but nothing happens")
          //update.left foreach { _(InitialSession(description)) }
          unit
        }
        unit
      }
      unit
    }
}

// def send(data: MessageBuffer) = channel.send(data.backingArrayBuffer)

class WebRTCChannelConnector(
    channel: dom.RTCDataChannel
) {

  def connect( /*connectionEstablished: Connected[WebRTC]*/ ) = {

    channel.onclose = { (_: dom.Event) =>
    }

    channel.onerror = { (_: dom.Event) =>
    }

    channel.onmessage = { (event: dom.MessageEvent) =>
      event.data match {
        case data: ArrayBuffer =>
          // TODO notify someone
          new JsArrayBufferMessageBuffer(data)

        case data: dom.Blob =>
          val reader = new dom.FileReader
          reader.onerror = { (event) =>
            println(s"reading message from blob returned error, event: $event")
          }
          reader.onload = { (event: dom.Event) =>
            val data = event.target.asInstanceOf[js.Dynamic].result.asInstanceOf[ArrayBuffer]
            new JsArrayBufferMessageBuffer(data)
          }
          reader.readAsArrayBuffer(data)

        case _ =>
      }
    }

    (channel.readyState) match {
      case dom.RTCDataChannelState.connecting =>
        // strange fix for strange issue with Chromium
        val handle = js.timers.setTimeout(1.day) { channel.readyState; () }

        channel.onopen = { (_: dom.Event) =>
          js.timers.clearTimeout(handle)
          println(s"connected!")
          // connectionEstablished.trySet(Success(connection))
        }

      case dom.RTCDataChannelState.open =>
      // connectionEstablished.trySet(Success(connection))

      case dom.RTCDataChannelState.closing | dom.RTCDataChannelState.closed =>
      // connectionEstablished.trySet(Failure(new ConnectionException("channel closed")))
    }

  }
}
