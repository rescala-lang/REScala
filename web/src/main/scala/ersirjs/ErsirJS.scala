package ersirjs

import ersir.shared._
import ersirjs.facade.ReMqtt
import ersirjs.render.Index
import loci.communicator.ws.akka.WS
import loci.registry.Registry
import loci.transmitter.RemoteRef
import org.scalajs.dom
import rescala.Tags._
import rescala.default._
import scalatags.JsDom.attrs.cls
import scalatags.JsDom.implicits._

import scala.concurrent.Future
import scala.scalajs.concurrent.JSExecutionContext.Implicits.queue
import scala.scalajs.js.annotation.{JSExport, JSExportTopLevel}
import scala.util.control.NonFatal


@JSExportTopLevel("Post")
object ErsirPost {
  var pgol: rescala.crdts.distributables.PGrowOnlyLog[Emergentcy] = null

  @JSExport
  def add(s: String): Unit = {
    try {
      val Array(title, desc) = s.split("\n", 2)
      pgol.prepend(Emergentcy(title, desc, ""))
    }
    catch {
      case NonFatal(_) => pgol.prepend(Emergentcy(s, "", ""))
    }
  }
}


object ErsirJS {

  val wsUri: String = {
    val wsProtocol = if (dom.document.location.protocol == "https:") "wss" else "ws"
    s"$wsProtocol://${dom.document.location.host}${dom.document.location.pathname}ws"
  }

  def main(args: Array[String]): Unit = {
    println("initing")


    val registry = new Registry
    val connection: Future[RemoteRef] = registry.connect(WS(wsUri))
    println(s"waiting for ws connection to server …")
    connection.foreach { remote =>
      val entryFuture = registry.lookup(Bindings.crdtDescriptions, remote)
      println(s"requesting $entryFuture")
      entryFuture.failed.foreach { t =>
        t.printStackTrace()
      }
      entryFuture.foreach { entryCrdt =>
        ErsirPost.pgol = entryCrdt
        val entrySignal = entryCrdt.valueSignal

        val emergencies = ReMqtt.topicstream("city/alert_state")
        val currentEmergency = emergencies.latest("")

        val manualStates = Evt[AppState]()

        val actions = new Actions(manualStates = manualStates)

        val connectClass = ReMqtt.connected.map {
          case true => "connected"
          case _    => ""
        }
        val index = new Index(actions, connectClass, entrySignal)
        val app = new ReaderApp()


        val bodySig = Signal {
          app.makeBody(index, manualStates).value.apply(cls := currentEmergency)
        }


        val bodyParent = dom.document.body.parentElement
        bodyParent.removeChild(dom.document.body)
        bodySig.asModifier.applyTo(bodyParent)
      }
    }
  }

}
