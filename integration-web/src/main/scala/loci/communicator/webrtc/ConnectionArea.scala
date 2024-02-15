package loci.communicator.webrtc

import channel.{ArrayMessageBuffer, Ctx}
import com.github.plokhotnyuk.jsoniter_scala.core.*
import com.github.plokhotnyuk.jsoniter_scala.macros.JsonCodecMaker
import de.rmgk.delay.syntax.toAsync
import de.rmgk.delay.{Async, Callback}
import loci.communicator.webrtc
import org.scalajs.dom
import org.scalajs.dom.{MouseEvent, RTCIceConnectionState, RTCIceServer, UIEvent, document, window}
import scalatags.JsDom.all.*
import scalatags.JsDom.tags2.section

import scala.concurrent.{ExecutionContext, Future, Promise}
import scala.scalajs.js
import scala.scalajs.js.{Array, Date}
import scala.scalajs.js.annotation.JSExportTopLevel
import scala.util.{Failure, Success}

object Example {

  // label seems mostly for auto negotiation
  val channelLabel = "loci-webrtc-channel"
  // id is used for pre negotiated channels
  val channelId: Double = 4

  @JSExportTopLevel("example")
  def example() = {
    val messages = div().render

    val stuff = input().render

    val renderedConnectionTable = table(
      tr(
        th("local session description"),
        th("remote session description"),
        th("iceGatheringState"),
        th("iceConnectionState"),
        th("signalingState"),
        th("trickle"),
      )
    ).render

    val renderedAddConnectionButton = button("new peerConnection").render

    Async.fromCallback {
      renderedAddConnectionButton.onclick = (ev: MouseEvent) =>
        Async.handler.succeed(())
    }.map: _ =>
      val handling = WebRTCHandling()
      renderedConnectionTable.appendChild(handling.controlRow().render)
      addDataChannel(messages, stuff, handling)
    .run(using ExecutionContext.global)(errorReporter)

    val para =
      section(
        p("some example text"),
        stuff,
        hr(),
        messages,
        hr(),
        renderedConnectionTable,
        renderedAddConnectionButton
      )

    document.body.replaceChild(para.render, document.body.firstChild)
  }

  def addDataChannel(messages: dom.html.Div, input: dom.html.Input, handling: WebRTCHandling) = {

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

        input.onchange = { _ =>
          println(s"trying to send stuff")
          connection.send(ArrayMessageBuffer(input.value.getBytes())).run(using ()):
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

    // there is also a listener for non negotiated channels
    // handling.peer.peerConnection.ondatachannel

    val channel = handling.peer.peerConnection.createDataChannel(
      Example.channelLabel,
      new dom.RTCDataChannelInit {
        negotiated = true
        id = Example.channelId
      }
    )
    WebRTCConnection.open(channel).run(using ())(handleConnection)

  }

}

def errorReporter: Callback[Any] =
  case Success(_) =>
  case Failure(ex) =>
    println(s"creating offer failed weirdly?")
    ex.printStackTrace()

case class WebRTCHandling() {

  val codec: JsonValueCodec[webrtc.SessionDescription] = JsonCodecMaker.make

  val peer = WebRTCConnector(new dom.RTCConfiguration {
    iceServers = js.Array[dom.RTCIceServer](new RTCIceServer {
      urls = js.Array[String]("stun:stun.t-online.de:3478")
    })
  })

  def controlRow(): ConcreteHtmlTag[dom.html.TableRow] = {

    val answerArea =
      textarea(
        placeholder := "remote session description",
        oninput := { (ev: UIEvent) =>
          try
            val cs = readFromString(ev.target.asInstanceOf[dom.html.TextArea].value)(codec)
            println(s"pending resolved, setting connector")
            peer.updateRemoteDescription(cs).run(using ())(errorReporter)
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

    val iceTrickle = textarea(readonly := true).render

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
      val lifecycle: ConnectorOverview = peer.lifecycle.bind
      lifecycle.localSession match
        case Some(s) => localSession.replaceChildren(sessionDisplay(s))
        case None    => localSession.replaceChildren(span("no local session info").render)

      lifecycle.remoteSession match
        case Some(s) => remoteSession.replaceChildren(sessionDisplay(s))
        case None    => remoteSession.replaceChildren(answerArea)

      gatheringState.innerText = lifecycle.iceGatheringState
      connectionState.innerText = lifecycle.iceConnectionState
      signalingState.innerText = lifecycle.signalingState
    }.run(using ())(errorReporter)

    Async {
      val dateOffset = Date.now()
      val candidate  = peer.iceCandidates.bind
      iceTrickle.value = s"${iceTrickle.value}${Date.now() - dateOffset}: ${candidate.candidate}\n\n"
    }.run(using ())(errorReporter)

    tr(localSession, remoteSession, gatheringState, connectionState, signalingState, td(iceTrickle))

  }

}
