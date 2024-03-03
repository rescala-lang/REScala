package todo

import channel.{ArrayMessageBuffer, Ctx, MessageBuffer}
import com.github.plokhotnyuk.jsoniter_scala.core.*
import com.github.plokhotnyuk.jsoniter_scala.macros.JsonCodecMaker
import de.rmgk.delay.{Async, Callback}
import loci.communicator.broadcastchannel.BroadcastChannelConnector
import loci.communicator.webrtc.{ConnectorOverview, SessionDescription, WebRTCConnection, WebRTCConnector}
import org.scalajs.dom
import org.scalajs.dom.html.{Div, Input, Table}
import org.scalajs.dom.{MouseEvent, RTCIceConnectionState, RTCIceGatheringState, RTCIceServer, UIEvent, document, window}
import rdts.dotted.Dotted
import reactives.operator.{Evt, Fold}
import scalatags.JsDom.all.*
import scalatags.JsDom.tags2.section

import scala.concurrent.{ExecutionContext, Future, Promise}
import scala.scalajs.js
import scala.scalajs.js.{Array, Date}
import scala.scalajs.js.annotation.JSExportTopLevel
import scala.util.{Failure, Random, Success}

sealed trait BroadcastCommunication
object BroadcastCommunication {
  case class Hello(id: Long)                                                        extends BroadcastCommunication
  case class Request(from: Long, to: Long, sessionDescription: SessionDescription)  extends BroadcastCommunication
  case class Response(from: Long, to: Long, sessionDescription: SessionDescription) extends BroadcastCommunication
}

given [T](using JsonValueCodec[T]): Conversion[MessageBuffer, T] = mb => readFromArray[T](mb.asArray)
given [T](using JsonValueCodec[T]): Conversion[T, MessageBuffer] = v => ArrayMessageBuffer(writeToArray[T](v))

given JsonValueCodec[BroadcastCommunication] = JsonCodecMaker.make




object Example {

  // label seems mostly for auto negotiation
  val channelLabel = "loci-webrtc-channel"
  // id is used for pre negotiated channels
  val channelId: Double = 4

  def example() = {
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
        val handling = WebRTCHandling(None)
        renderedConnectionTable.appendChild(handling.controlRow().render)
        addDataChannel(handling)
    .run(using ExecutionContext.global)(errorReporter)

    useLocalBroadcastChannel(renderedConnectionTable)

    val para =
      section(
        hr(),
        renderedConnectionTable,
        renderedAddConnectionButton
      )

    para
  }

  private def useLocalBroadcastChannel(renderedConnectionTable: Table) = {
    val broadcast = BroadcastChannelConnector("channels local broadcast for webrtc offers")

    var autoconnections: Map[Long, WebRTCHandling] = Map.empty
    val selfId                                     = Random.nextLong()

    Async[Ctx] {
      val msg                                   = broadcast.receive.bind
      val communication: BroadcastCommunication = msg.convert
      Async[Ctx].bind:
        Async[Ctx].fromCallback:
          communication match
            case BroadcastCommunication.Hello(id) =>
              val handling = WebRTCHandling(Some {
                case Success(sd) =>
                  broadcast.send(BroadcastCommunication.Request(selfId, id, sd)).run(Async.handler)
              })
              autoconnections = autoconnections.updated(id, handling)
              renderedConnectionTable.appendChild(handling.controlRow().render)
              addDataChannel(handling)

            case BroadcastCommunication.Request(from, `selfId`, sessionDescription) =>
              val handling = WebRTCHandling(Some {
                case Success(sd) =>
                  broadcast.send(BroadcastCommunication.Response(selfId, from, sd)).run(Async.handler)
              })
              autoconnections = autoconnections.updated(from, handling)
              renderedConnectionTable.appendChild(handling.controlRow().render)
              addDataChannel(handling)
              handling.peer.updateRemoteDescription(sessionDescription).run(Async.handler)
            case BroadcastCommunication.Response(from, `selfId`, sessionDescription) =>
              autoconnections.get(from).foreach: handling =>
                handling.peer.updateRemoteDescription(sessionDescription).run(Async.handler)
    }.run(using Ctx())(errorReporter)

    broadcast.send(BroadcastCommunication.Hello(selfId).convert).run(using ())(errorReporter)

  }
  def addDataChannel(handling: WebRTCHandling) = {

    def handleConnection: Callback[WebRTCConnection] =
      case Success(connection) =>
        println("adding connection!!!!")
        connection.receive.run(using Ctx()):
          case Success(msg) =>
            GlobalRegistry.handle(msg)
            ()
          case Failure(err) =>
            println(s"oh noes")
            err.printStackTrace()
            handling.peer.peerConnection.asInstanceOf[js.Dynamic].restartIce()
            ()

        GlobalRegistry.addConnection(connection)

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

class WebRTCHandling(readyChannel: Option[Callback[SessionDescription]]) {

  val codec: JsonValueCodec[SessionDescription] = JsonCodecMaker.make

  val peer = WebRTCConnector(new dom.RTCConfiguration {
    iceServers = js.Array[dom.RTCIceServer](new RTCIceServer {
      urls = js.Array[String]("stun:stun.t-online.de:3478")
    })
  })

  var readyChannelSent = false

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

      if !readyChannelSent && lifecycle.iceGatheringState == RTCIceGatheringState.complete then
        readyChannelSent = true
        readyChannel.foreach(cb => lifecycle.localSession.foreach(cb.succeed))

    }.run(using ())(errorReporter)

    Async {
      val dateOffset = Date.now()
      val candidate  = peer.iceCandidates.bind
      iceTrickle.value = s"${iceTrickle.value}${Date.now() - dateOffset}: ${candidate.candidate}\n\n"
    }.run(using ())(errorReporter)

    tr(localSession, remoteSession, gatheringState, connectionState, signalingState, td(iceTrickle))

  }

}
