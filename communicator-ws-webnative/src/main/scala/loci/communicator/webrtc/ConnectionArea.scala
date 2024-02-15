package loci.communicator.webrtc

import channel.{ArrayMessageBuffer, Ctx}
import com.github.plokhotnyuk.jsoniter_scala.core.*
import com.github.plokhotnyuk.jsoniter_scala.macros.JsonCodecMaker
import de.rmgk.delay.Callback
import loci.communicator.webrtc
import org.scalajs.dom
import org.scalajs.dom.{RTCIceConnectionState, UIEvent, document, window}
import scalatags.JsDom.all.*
import scalatags.JsDom.tags2.section

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.{Future, Promise}
import scala.scalajs.js
import scala.scalajs.js.Array
import scala.scalajs.js.annotation.JSExportTopLevel
import scala.util.{Failure, Success}

object Example {
  @JSExportTopLevel("example")
  def example() = {
    val handling = WebRTCHandling()

    val messages = div().render

    val stuff = input().render

    def handleConnection: Callback[WebRTCConnection] =
      case Success(connection) =>
        println("adding connection!!!!")
        connection.receive.run(using Ctx()):
          case Success(msg) =>
            val res = new String(msg.asArray)
            println(s"received $res")
            messages.appendChild(p(res).render)
            ()
          case Failure(err) =>
            println(s"oh noes")
            err.printStackTrace()
            handling.peer.peerConnection.asInstanceOf[js.Dynamic].restartIce()
            ()

        stuff.onchange = { _ =>
          println(s"trying to send stuff")
          connection.send(ArrayMessageBuffer(stuff.value.getBytes())).run(using ()):
            case Success(_) => println("sent")
            case Failure(ex) =>
              println(s"sending failed")
              ex.printStackTrace()
              handling.peer.peerConnection.asInstanceOf[js.Dynamic].restartIce()
              ()
        }

      case Failure(err) =>
        println(s"oh noes")
        err.printStackTrace()
        handling.peer.peerConnection.asInstanceOf[js.Dynamic].restartIce()
        ()

    val channel = handling.peer.peerConnection.createDataChannel(
      WebRTCConnector.channelLabel,
      new dom.RTCDataChannelInit {
        negotiated = true
        id = WebRTCConnector.channelId
      }
    )
    WebRTCConnection.open(channel).run(using ())(handleConnection)

    // handling.peer.incomingDataChannel.run(using ())(handleConnection)

    val area = handling.webrtcHandlingArea()

    val para = section(p("some example text"), stuff, p(), messages, area)

    document.body.replaceChild(para.render, document.body.firstChild)
  }
}

case class WebRTCHandling() {

  val codec: JsonValueCodec[webrtc.SessionDescription] = JsonCodecMaker.make

  val peer = WebRTCConnector(new dom.RTCConfiguration { iceServers = js.Array[dom.RTCIceServer]() })

  def webrtcHandlingArea(): Tag = {

    val renderedTa    = textarea().render
    val renderedPre   = pre().render
    val renderedState = p().render
    val iceThings     = p().render

    renderedState.innerText = peer.peerConnection.signalingState
    peer.peerConnection.onsignalingstatechange = { _ =>
      renderedState.innerText = peer.peerConnection.signalingState
    }

    iceThings.innerText = peer.peerConnection.iceConnectionState
    peer.peerConnection.oniceconnectionstatechange = { ev =>
      iceThings.innerText = peer.peerConnection.iceConnectionState
      if (peer.peerConnection.iceConnectionState == RTCIceConnectionState.failed) {
        peer.peerConnection.asInstanceOf[js.Dynamic].restartIce()
        ()
      }
      if (peer.peerConnection.iceConnectionState == RTCIceConnectionState.connected) then
        connected()
    }

    def connected() = {
      renderedPre.textContent = ""
      renderedTa.value = ""
    }

    def showSession(s: SessionDescription) = {
      val message = writeToString(s)(codec)
      println(s"completed offer $message")
      renderedPre.textContent = message
      org.scalajs.dom.window.getSelection().selectAllChildren(renderedPre)
    }

    def handleSession: Callback[SessionDescription] =
      case Success(session) => showSession(session)
      case Failure(ex) =>
        println("failed ICE, should not be possible")
        ex.printStackTrace()

    val hb = button(
      "offer",
      onclick := { (_: UIEvent) =>
        peer.offer(new dom.RTCOfferOptions {}).run(using ())(handleSession)
      }
    )

    val cb = button(
      "accept",
      onclick := { (_: UIEvent) =>
        val cs = readFromString(renderedTa.value)(codec)
        println(s"pending resolved, setting connector")
        peer.accept(cs).run(using ())(handleSession)
      }
    )

    section(renderedState, iceThings, hb, cb, renderedPre, renderedTa)
  }

}
