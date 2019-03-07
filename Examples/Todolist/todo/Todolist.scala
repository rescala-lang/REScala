package todo

import io.circe.generic.auto._
import io.circe.syntax._
import loci.communicator.experimental.webrtc.WebRTC.ConnectorFactory
import loci.communicator.experimental.webrtc._
import loci.registry.{Binding, Registry}
import loci.serializer.circe._
import org.scalajs.dom.{UIEvent, document}
import rescala.debuggable.ChromeDebuggerInterface
import rescala.restoration.LocalStorageStore
import rescala.lattices.sequences.RGOA
import scalatags.JsDom.all._
import scalatags.JsDom.tags2.section

import scala.concurrent.{Future, Promise}
import scala.scalajs.concurrent.JSExecutionContext.Implicits.queue


object Todolist {


  type TodoTransfer = RGOA[taskHandling.Taskref]

  implicit val storingEngine: LocalStorageStore = new LocalStorageStore()
  import storingEngine._

  val taskHandling = new TaskHandling
  val todoApp = TodoApp(taskHandling)

  val registry = new Registry
  val crdtDescriptions = Binding[TodoTransfer => Unit]("taskData")

  val remotePromise = Evt[TodoTransfer => Future[Unit]]

  def main(args: Array[String]): Unit = {

    ChromeDebuggerInterface.setup(storingEngine)

    val remote = Events.fromCallback[RGOA[taskHandling.Taskref]](
      cb => registry.bind(crdtDescriptions) { td: TodoTransfer =>
      cb(td)
    })

    remote.event.observe(tl => println(s"received $tl"))

    val todores = todoApp.getContents(remote.event)

    document.body.replaceChild(todores.div.render, document.body.firstElementChild)
    document.body.appendChild(webrtchandlingArea.render)

    remotePromise.observe { f =>
      todores.tasklist.observe{s =>
        println(s"sending tasklist $s")
        f(s)
      }
    }

    ChromeDebuggerInterface.finishedLoading()
  }




  def webrtchandlingArea: Tag = {

    val renderedTa = textarea().render
    val renderedPre = pre().render

    var pn: Option[PendingNonesense] = None

    val hb = button("host", onclick := { uie: UIEvent =>
      val res = webrtcIntermediate(WebRTC.offer())
      res.session.foreach(s => renderedPre.textContent = s.asJson.noSpaces)
      pn = Some(res)
      val remoteFut = registry.connect(res.connector)
      remoteFut.foreach { remote =>
        val res = registry.lookup(crdtDescriptions, remote)
        println(s"host connected $res")
        remotePromise.fire(res)
      }
    })


    val cb = button("connect", onclick := { uie: UIEvent =>
      val cs = io.circe.parser.decode[WebRTC.CompleteSession](renderedTa.value).right.get
      (pn match {
        case None     => // we are client
          val res = webrtcIntermediate(WebRTC.answer())
          res.session.foreach(s => renderedPre.textContent = s.asJson.noSpaces)
          registry.connect(res.connector)
          res.connector
        case Some(ss) => // we are server
          ss.connector
      }) set cs
    })

    section(hb, cb, renderedPre, renderedTa)
  }




  case class PendingNonesense(connector: WebRTC.Connector,
                         session: Future[WebRTC.CompleteSession])

  def webrtcIntermediate(cf: ConnectorFactory) = {
    val p = Promise[WebRTC.CompleteSession]()
    val answer = cf complete p.success
    PendingNonesense(answer, p.future)
  }


}
