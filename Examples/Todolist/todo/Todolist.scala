package todo

import io.circe.generic.auto._
import io.circe.syntax._
import loci.communicator.experimental.webrtc.WebRTC.ConnectorFactory
import loci.communicator.experimental.webrtc._
import loci.registry.{Binding, Registry}
import loci.serializer.circe._
import loci.transmitter.RemoteRef
import org.scalajs.dom.{UIEvent, document}
import rescala.Tags._
import rescala.debuggable.ChromeDebuggerInterface
import rescala.lattices.sequences.{RGA, RGOA}
import rescala.locidistribute.LociDist
import rescala.restoration.LocalStorageStore
import rescala.restoration.ReCirce._
import scalatags.JsDom.all._
import scalatags.JsDom.tags2.section

import scala.concurrent.{Future, Promise}
import scala.scalajs.concurrent.JSExecutionContext.Implicits.queue
import scala.util.Try


object Todolist {


  type TodoTransfer = RGA[taskHandling.Taskref]

  implicit val storingEngine: LocalStorageStore = new LocalStorageStore()
  import storingEngine._

  val taskHandling = new TaskHandling
  val todoApp = TodoApp(taskHandling)

  val registry = new Registry
  val crdtDescriptions = Binding[TodoTransfer => Unit]("taskData")

  val remoteConnected = Evt[RemoteRef]



  def main(args: Array[String]): Unit = {

    ChromeDebuggerInterface.setup(storingEngine)

    val remote = Events.fromCallback[TodoTransfer](
      cb => registry.bind(crdtDescriptions) { td: TodoTransfer =>
        println(s"receiving tasklist $td")
        Try{cb(td)}.failed.foreach{println}
        println(s"caled cb")
      })

    remote.event.observe(tl => println(s"received $tl"))

    val todores = todoApp.getContents(remote.event)

    val meh = Evt[String]
    val mehs = meh.fold(RGOA.empty[String])((c, e) => c.append(e))("meh", implicitly)
    LociDist.distribute(mehs, registry, storingEngine.scheduler)

    meh.fire(s"meh ${System.currentTimeMillis()}")



    document.body.replaceChild(todores.div.render, document.body.firstElementChild)
    document.body.appendChild(webrtchandlingArea.render)
    document.body.appendChild(div(mehs.map(r => ul(r.value.map(li(_)))).asModifier).render)


    remoteConnected.observe { remote =>
      println(s"host connected $remote")
      val res = registry.lookup(crdtDescriptions, remote)
      todores.tasklist.observe { s =>
        println(s"sending tasklist $s")
        res(s)
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
      remoteFut.foreach(remoteConnected.fire(_))
    })


    val cb = button("connect", onclick := { uie: UIEvent =>
      val cs = io.circe.parser.decode[WebRTC.CompleteSession](renderedTa.value).right.get
      (pn match {
        case None     => // we are client
          val res = webrtcIntermediate(WebRTC.answer())
          res.session.foreach(s => renderedPre.textContent = s.asJson.noSpaces)
          registry.connect(res.connector).foreach(remoteConnected.fire(_))
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
