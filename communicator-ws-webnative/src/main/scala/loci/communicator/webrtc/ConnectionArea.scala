package loci.communicator.webrtc

import channel.{ArrayMessageBuffer, Ctx}
import com.github.plokhotnyuk.jsoniter_scala.core.*
import com.github.plokhotnyuk.jsoniter_scala.macros.JsonCodecMaker
import de.rmgk.delay.{Async, Callback}
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

    def errorReporter: Callback[Any] =
      case Success(_) =>
      case Failure(ex) =>
        println(s"creating offer failed weirdly?")
        ex.printStackTrace()

    val offerButton = button(
      "offer",
      onclick := { (_: UIEvent) =>
        peer.offer().run(using ())(errorReporter)
      }
    ).render

    val answerArea =
      textarea(
        placeholder := "remote session description",
        oninput := { (ev: UIEvent) =>
          try
            val cs = readFromString(ev.target.asInstanceOf[dom.html.TextArea].value)(codec)
            println(s"pending resolved, setting connector")
            peer.accept(cs).run(using ())(errorReporter)
          catch
            case _: JsonReaderException =>
              println(s"input is not a valid session description")
        }
      ).render

    val localSession    = td().render
    val remoteSession   = td().render
    val gatheringState  = td().render
    val connectionState = td().render
    val signalingState  = td().render

    def sessionDisplay(sessionDescription: SessionDescription): dom.html.TextArea = {
      textarea(
        readonly := true,
        onfocus := { (ev: UIEvent) =>
          ev.target.asInstanceOf[dom.html.TextArea].select()
        },
        writeToString(sessionDescription)(codec)
      ).render
    }

    Async[Any] {
      val lifecycle: ConnectorLifecycle = peer.lifecycle.bind
      lifecycle.localSession match
        case Some(s) => localSession.replaceChildren(sessionDisplay(s))
        case None    => localSession.replaceChildren(offerButton)

      lifecycle.remoteSession match
        case Some(s) => remoteSession.replaceChildren(sessionDisplay(s))
        case None    => remoteSession.replaceChildren(answerArea.render)

      gatheringState.innerText = lifecycle.iceGatheringState
      connectionState.innerText = lifecycle.iceConnectionState
      signalingState.innerText = lifecycle.signalingState
    }.run(using ())(errorReporter)

    val res = table(
      tr(
        th("local session description"),
        th("remote session description"),
        th("iceGatheringState"),
        th("iceConnectionState"),
        th("signalingState")
      ),
      tr(localSession, remoteSession, gatheringState, connectionState, signalingState)
    )

    res

  }

}
