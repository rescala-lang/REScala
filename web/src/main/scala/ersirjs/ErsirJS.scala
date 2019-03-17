package ersirjs

import ersir.shared._
import ersirjs.facade.ReMqtt
import ersirjs.render.Index
import io.circe.generic.auto._
import io.circe.syntax._
import loci.communicator.ws.akka.WS
import loci.registry.Registry
import loci.transmitter.RemoteRef
import org.scalajs.dom
import rescala.Tags._
import rescala.default._
import rescala.lattices.Lattice
import rescala.lattices.sequences.RGOA.RGOA
import scalatags.JsDom.attrs.cls
import scalatags.JsDom.implicits._
import io.circe.parser.decode

import scala.concurrent.Future
import scala.scalajs.concurrent.JSExecutionContext.Implicits.queue
import scala.scalajs.js.annotation.{JSExport, JSExportTopLevel}
import scala.util.control.NonFatal


@JSExportTopLevel("Post")
object ErsirPost {
  var pgol: rescala.distributables.PGrowOnlyLog[Posting] = null

  @JSExport
  def add(s: String, url: String = ""): Unit = {
    try {
      val Array(title, desc) = s.split("\n", 2)
      pgol.prepend(Posting(title, desc, url, System.currentTimeMillis()))
    }
    catch {
      case NonFatal(_) => pgol.prepend(Posting(s, "", url, System.currentTimeMillis()))
    }
  }
}


object ErsirJS {

  val wsUri: String = {
    val wsProtocol = if (dom.document.location.protocol == "https:") "wss" else "ws"
    s"$wsProtocol://${dom.document.location.host}${dom.document.location.pathname}ws"
  }

  def main(args: Array[String]): Unit = {
    println("initing 2")


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

        ReMqtt.start()

        entryCrdt.crdtSignal.observe { crdt =>
          if (ReMqtt.isConnected()) {
            val json = crdt.asJson.noSpaces
            ReMqtt.send("ersir/entries", json)
          }
        }


        val entryStream = ReMqtt
                          .topicstream("ersir/entries")
                          .map(decode[RGOA[Posting]])
                          .collect { case Right(rg) => rg }

        entryStream.observe(rg => entryCrdt.transform(Lattice.merge(_, rg)))

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
