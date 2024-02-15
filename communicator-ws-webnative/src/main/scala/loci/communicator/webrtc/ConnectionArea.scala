package loci.communicator.webrtc

import channel.{ArrayMessageBuffer, Ctx}
import com.github.plokhotnyuk.jsoniter_scala.core.*
import com.github.plokhotnyuk.jsoniter_scala.macros.JsonCodecMaker
import de.rmgk.delay.Callback
import loci.communicator.webrtc
import org.scalajs.dom
import org.scalajs.dom.{UIEvent, document, window}
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
        connection.receive.run(using Ctx()):
          case Success(msg) =>
            val res = new String(msg.asArray)
            println(s"received $res")
            messages.appendChild(p(res).render)
            ()
          case Failure(err) =>
            println(s"oh noes")
            err.printStackTrace()

        stuff.onchange = { _ =>
          println(s"trying to send stuff")
          connection.send(ArrayMessageBuffer(stuff.value.getBytes())).run(using ()):
            case Success(_) => println("sent")
            case Failure(ex) =>
              println(s"sending failed")
              ex.printStackTrace()
        }

      case Failure(err) =>
        println(s"oh noes")
        err.printStackTrace()

    handling.peer.incomingDataChannel.run(using ())(handleConnection)

    val area = handling.webrtcHandlingArea(handleConnection)

    val para = section(p("some example text"), stuff, messages, area)

    document.body.replaceChild(para.render, document.body.firstChild)
  }
}

case class WebRTCHandling() {

  val codec: JsonValueCodec[webrtc.CompleteSession] = JsonCodecMaker.make

  val peer = WebRTCConnector(new dom.RTCConfiguration { iceServers = js.Array[dom.RTCIceServer]() })

  def webrtcHandlingArea(callback: Callback[WebRTCConnection]): Tag = {

    val renderedTa  = textarea().render
    val renderedPre = pre().render

    var pendingServer: Boolean = false

    def connected() = {
      renderedPre.textContent = ""
      renderedTa.value = ""
    }

    def showSession(s: CompleteSession) = {
      val message = writeToString(s)(codec)
      println(s"completed offer $message")
      renderedPre.textContent = message
      org.scalajs.dom.window.getSelection().selectAllChildren(renderedPre)
    }

    peer.iceCandidates.run(using ()):
      case Success(session) => showSession(session)
      case Failure(ex) =>
        println("failed ICE, should not be possible")
        ex.printStackTrace()

    val hb = button(
      "host",
      onclick := { (_: UIEvent) =>
        peer.connect(new dom.RTCOfferOptions {}).run(using ())(callback)
      }
    )

    val cb = button(
      "connect",
      onclick := { (_: UIEvent) =>
        val cs = readFromString(renderedTa.value)(codec)
        println(s"pending resolved, setting connector")
        peer.set(cs)
      }
    )

    section(hb, cb, renderedPre, renderedTa)
  }

}
