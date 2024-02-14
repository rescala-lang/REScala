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
    update: Either[IncrementalUpdate => Unit, CompleteSession => Unit]
) extends WebRTC.Connector {

  val peerConnection = new dom.RTCPeerConnection(configuration)

  peerConnection.onicecandidate = { (event: dom.RTCPeerConnectionIceEvent) =>
    if (event.candidate != null)
      update.left foreach {
        _(SessionUpdate(event.candidate))
      }
    else
      update.foreach {
        _(CompleteSession(peerConnection.localDescription))
      }
  }

//  private var connectionCount = 0
//
//  protected def handleConnectionClosing(connection: Try[Connection[WebRTC]]) = {
//    connection foreach { connection =>
//      connectionCount += 1
//      connection.closed foreach { _ =>
//        connectionCount -= 1
//        if (connectionCount < 1)
//          peerConnection.close()
//      }
//    }
//  }

  private var remoteDescriptionSet = false

  def connection = peerConnection

  def use(update: IncrementalUpdate) = update match {
    case session: InitialSession =>
      if (!remoteDescriptionSet) {
        remoteDescriptionSet = true
        setRemoteDescription(session.sessionDescription)
      }
    case update: SessionUpdate =>
      peerConnection.addIceCandidate(update.iceCandidate)
      ()
  }

  def set(update: CompleteUpdate) = update match {
    case session: CompleteSession =>
      if (!remoteDescriptionSet) {
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
    update: Either[IncrementalUpdate => Unit, CompleteSession => Unit]
) extends WebRTCConnector(configuration, update) {

  protected def connect( /*connectionEstablished: Connected[WebRTC]*/ ) =
    try {
      val channel = peerConnection.createDataChannel(
        WebRTCConnector.channelLabel,
        new dom.RTCDataChannelInit {}
      )

      peerConnection.createOffer(options) `then` { (description: dom.RTCSessionDescription) =>
        peerConnection.setLocalDescription(description) `then` { (_: Unit) =>
          update.left foreach { _(InitialSession(description)) }
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
    update: Either[IncrementalUpdate => Unit, CompleteSession => Unit]
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
          update.left foreach { _(InitialSession(description)) }
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
          // connectionEstablished.trySet(Success(connection))
        }

      case dom.RTCDataChannelState.open =>
      // connectionEstablished.trySet(Success(connection))

      case dom.RTCDataChannelState.closing | dom.RTCDataChannelState.closed =>
      // connectionEstablished.trySet(Failure(new ConnectionException("channel closed")))
    }

  }
}
