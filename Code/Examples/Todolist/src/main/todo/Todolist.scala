package todo

import io.circe.generic.auto._
import io.circe.syntax._
import loci.communicator.experimental.webrtc.WebRTC.ConnectorFactory
import loci.communicator.experimental.webrtc._
import loci.registry.Registry
import org.scalajs.dom.{UIEvent, document}
import scalatags.JsDom.all._
import scalatags.JsDom.tags2.section

import scala.concurrent.{Future, Promise}
import scala.scalajs.concurrent.JSExecutionContext.Implicits.queue

object Todolist {

  val todoApp = new TodoApp()

  val registry = new Registry

  def main(args: Array[String]): Unit = {

    val div = todoApp.getContents()

    document.body.replaceChild(div.render, document.body.firstElementChild)
    document.body.appendChild(webrtcHandlingArea.render)

  }

  def webrtcHandlingArea: Tag = {

    val renderedTa  = textarea().render
    val renderedPre = pre().render

    var pendingServer: Option[PendingConnection] = None

    def connected() = {
      renderedPre.textContent = ""
      renderedTa.value = ""
    }

    def showSession(s: WebRTC.CompleteSession) = {
      val message = s.asJson.noSpaces: @scala.annotation.nowarn
      renderedPre.textContent = message
      org.scalajs.dom.window.getSelection().selectAllChildren(renderedPre)
    }

    val hb = button(
      "host",
      onclick := { uie: UIEvent =>
        val res = webrtcIntermediate(WebRTC.offer())
        res.session.foreach(showSession)
        pendingServer = Some(res)
        registry.connect(res.connector).foreach(_ => connected())
      }
    )

    val cb = button(
      "connect",
      onclick := { uie: UIEvent =>
        val cs =
          io.circe.parser.decode[WebRTC.CompleteSession](
            renderedTa.value
          ).right.get: @scala.annotation.nowarn // 2.12 compat
        val connector = pendingServer match {
          case None => // we are client
            val res = webrtcIntermediate(WebRTC.answer())
            res.session.foreach(showSession)
            registry.connect(res.connector).foreach(_ => connected())
            res.connector
          case Some(ss) => // we are server
            pendingServer = None
            ss.connector
        }
        connector.set(cs)
      }
    )

    section(hb, cb, renderedPre, renderedTa)
  }

  case class PendingConnection(connector: WebRTC.Connector, session: Future[WebRTC.CompleteSession])

  def webrtcIntermediate(cf: ConnectorFactory) = {
    val p      = Promise[WebRTC.CompleteSession]()
    val answer = cf complete p.success
    PendingConnection(answer, p.future)
  }

}
