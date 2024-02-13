package loci
package communicator
package webrtc

import WebRTC.{CompleteSession, CompleteUpdate, IncrementalUpdate, InitialSession, SessionUpdate}

import org.scalajs.dom

import scala.concurrent.duration._
import scala.scalajs.js
import scala.scalajs.js.typedarray.ArrayBuffer
import scala.util.{Failure, Success, Try}
import scala.util.control.NonFatal

private object WebRTCConnector {
  val channelLabel = "loci-webrtc-channel"
}

private abstract class WebRTCConnector(
  configuration: dom.RTCConfiguration,
  update: Either[IncrementalUpdate => Unit, CompleteSession => Unit])
    extends WebRTC.Connector {

  val peerConnection = new dom.RTCPeerConnection(configuration)

  peerConnection.onicecandidate = { (event: dom.RTCPeerConnectionIceEvent) =>
    if (event.candidate != null)
      update.left foreach {
        _(SessionUpdate(event.candidate))
      }
    else
      compatibility.either.foreach(update) {
        _(CompleteSession(peerConnection.localDescription))
      }
  }

  private var connectionCount = 0

  protected def handleConnectionClosing(connection: Try[Connection[WebRTC]]) = {
    connection foreach { connection =>
      connectionCount += 1
      connection.closed foreach { _ =>
        connectionCount -= 1
        if (connectionCount < 1)
          peerConnection.close()
      }
    }
  }

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

private class WebRTCOffer(
  configuration: dom.RTCConfiguration,
  options: dom.RTCOfferOptions,
  update: Either[IncrementalUpdate => Unit, CompleteSession => Unit])
    extends WebRTCConnector(configuration, update) {

  protected def connect(connectionEstablished: Connected[WebRTC]) =
    try {
      val channel = peerConnection.createDataChannel(
        WebRTCConnector.channelLabel,
        new dom.RTCDataChannelInit { })

      peerConnection.createOffer(options) `then` { (description: dom.RTCSessionDescription) =>
        peerConnection.setLocalDescription(description) `then` { (_: Unit) =>
          update.left foreach { _(InitialSession(description)) }
          unit
        }
        unit
      }

      new WebRTCChannelConnector(channel, Some(this)).connect() { connection =>
        handleConnectionClosing(connection)
        connectionEstablished.trySet(connection)
      }
    }
    catch {
      case NonFatal(exception) =>
        connectionEstablished.set(Failure(exception))
    }

  protected def setRemoteDescription(description: dom.RTCSessionDescription) =
    peerConnection.setRemoteDescription(description)
}

private class WebRTCAnswer(
  configuration: dom.RTCConfiguration,
  update: Either[IncrementalUpdate => Unit, CompleteSession => Unit])
    extends WebRTCConnector(configuration, update) {

  private val connectorQueue = new js.Array[Connector[WebRTC]](0)
  private val connectedQueue = new js.Array[Connected[WebRTC]](0)

  private def connect() = 
    if (connectorQueue.length != 0 && connectedQueue.length != 0) {
      val connector = connectorQueue.shift()
      val connected = connectedQueue.shift()
      connector.connect() { connection =>
        handleConnectionClosing(connection)
        connected.trySet(connection)
      }
    }

  protected def connect(connectionEstablished: Connected[WebRTC]) = {
    connectedQueue.push(connectionEstablished)
    connect()
  }

  peerConnection.ondatachannel = { (event: dom.RTCDataChannelEvent) =>
    if (event.channel.label == WebRTCConnector.channelLabel) {
      connectorQueue.push(new WebRTCChannelConnector(event.channel, Some(this)))
      connect()
    }
  }

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

private class WebRTCChannelConnector(
  channel: dom.RTCDataChannel,
  optionalConnectionSetup: Option[ConnectionSetup[WebRTC]])
    extends Connector[WebRTC] {

  protected def connect(connectionEstablished: Connected[WebRTC]) = {
    val legacyReliable = {
      val rel = channel.asInstanceOf[js.Dynamic].reliable
      !js.isUndefined(rel) && rel.asInstanceOf[Boolean]
    }
    val reliable = (
      legacyReliable
        || channel.ordered
        || (js.isUndefined(channel.maxPacketLifeTime)
          && js.isUndefined(channel.maxRetransmits))
      )

    if (reliable) {
      val connection = {
        val doClosed = Notice.Steady[Unit]
        val doReceive = Notice.Stream[MessageBuffer]

        val connection = new Connection[WebRTC] {
          val protocol = new WebRTC {
            val setup = optionalConnectionSetup getOrElse WebRTCChannelConnector.this
            val authenticated = false
          }

          val closed = doClosed.notice
          val receive = doReceive.notice

          var open = true
          def send(data: MessageBuffer) = channel.send(data.backingArrayBuffer)
          def close() = if (open) {
            open = false
            channel.close()
            doClosed.set()
          }
        }

        channel.onclose = { (_: dom.Event) =>
          connectionEstablished.trySet(Failure(new ConnectionException("channel closed")))
          connection.close()
        }

        channel.onerror = { (_: dom.Event) =>
          connectionEstablished.trySet(Failure(new ConnectionException("channel closed")))
          connection.close()
        }

        channel.onmessage = { (event: dom.MessageEvent) =>
          event.data match {
            case data: ArrayBuffer =>
              doReceive.fire(MessageBuffer wrapArrayBuffer data)

            case data: dom.Blob =>
              val reader = new dom.FileReader
              reader.onload = { (event: dom.Event) =>
                doReceive.fire(MessageBuffer wrapArrayBuffer
                  event.target.asInstanceOf[js.Dynamic].result.asInstanceOf[ArrayBuffer])
              }
              reader.readAsArrayBuffer(data)

            case _ =>
          }
        }

        connection
      }

      (channel.readyState: @unchecked) match {
        case dom.RTCDataChannelState.connecting =>
          // strange fix for strange issue with Chromium
          val handle = js.timers.setTimeout(1.day) { channel.readyState }
  
          channel.onopen = { (_: dom.Event) =>
            js.timers.clearTimeout(handle)
            connectionEstablished.trySet(Success(connection))
          }
  
        case dom.RTCDataChannelState.open =>
          connectionEstablished.trySet(Success(connection))

        case dom.RTCDataChannelState.closing | dom.RTCDataChannelState.closed =>
          connectionEstablished.trySet(Failure(new ConnectionException("channel closed")))
      }
    }
    else
      connectionEstablished.trySet(Failure(new ConnectionException("channel unreliable")))
  }
}
