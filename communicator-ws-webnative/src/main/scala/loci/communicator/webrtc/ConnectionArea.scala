package loci.communicator.webrtc

import com.github.plokhotnyuk.jsoniter_scala.core.*
import com.github.plokhotnyuk.jsoniter_scala.macros.JsonCodecMaker
import loci.communicator.webrtc
import loci.communicator.webrtc.WebRTC
import loci.communicator.webrtc.WebRTC.ConnectorFactory
import org.scalajs.dom.{UIEvent, document}
import scalatags.JsDom.all.*
import scalatags.JsDom.tags2.section

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.{Future, Promise}
import scala.scalajs.js.annotation.JSExportTopLevel
import scala.util.{Failure, Success}

object Example {
  @JSExportTopLevel("example")
  def example() = {
    val handling = WebRTCHandling()
    val area     = handling.webrtcHandlingArea()

    val para = section(p("some example text"), input(), area)


    document.body.replaceChild(para.render, document.body.firstChild)
  }
}

case class WebRTCHandling() {

  val codec: JsonValueCodec[webrtc.WebRTC.CompleteSession] = JsonCodecMaker.make

  def webrtcHandlingArea: Tag = {

    val renderedTa  = textarea().render
    val renderedPre = pre().render

    var pendingServer: Option[PendingConnection] = None

    def connected() = {
      renderedPre.textContent = ""
      renderedTa.value = ""
    }

    def showSession(s: WebRTC.CompleteSession) = {
      val message = writeToString(s)(codec)
      println(s"completed offer $message")
      renderedPre.textContent = message
      org.scalajs.dom.window.getSelection().selectAllChildren(renderedPre)
    }

    val hb = button(
      "host",
      onclick := { (_: UIEvent) =>
        val res = webrtcIntermediate(WebRTC.offer())
        println(s"generated offer $res")
        res.session.onComplete:
          case Success(session) => showSession(session)
          case Failure(ex) =>
            println(s"received: ")
            ex.printStackTrace()
        pendingServer = Some(res)
        // registry.connect(res.connector).foreach(_ => connected())
      }
    )

    val cb = button(
      "connect",
      onclick := { (_: UIEvent) =>
        val cs = readFromString(renderedTa.value)(codec)
        val connector = pendingServer match {
          case None => // we are client
            val res = webrtcIntermediate(WebRTC.answer())
            res.session.foreach(showSession)
            // registry.connect(res.connector).foreach(_ => connected())
            res.connector
          case Some(ss) => // we are server
            pendingServer = None
            ss.connector
        }
        println(s"pending resolved, setting connector")
        connector.set(cs)
      }
    )

    section(hb, cb, renderedPre, renderedTa)
  }

  case class PendingConnection(connector: WebRTC.Connector, session: Future[WebRTC.CompleteSession])

  def webrtcIntermediate(cf: ConnectorFactory) = {
    val p      = Promise[WebRTC.CompleteSession]()
    val answer = cf.complete(p.success)
    PendingConnection(answer, p.future)
  }

}
