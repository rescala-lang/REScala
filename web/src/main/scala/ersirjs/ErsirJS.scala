package ersirjs

import ersir.shared._
import ersirjs.render.Index
import loci.communicator.ws.akka.WS
import loci.registry.Registry
import loci.transmitter.RemoteRef
import org.scalajs.dom
import rescala.default._
import rescala.rescalatags._
import scalatags.JsDom.implicits.stringFrag
import scalatags.JsDom.tags.body

import scala.concurrent.Future
import scala.scalajs.concurrent.JSExecutionContext.Implicits.queue


object ErsirJS {

  val wsUri: String = {
    val wsProtocol = if (dom.document.location.protocol == "https:") "wss" else "ws"
    s"$wsProtocol://${dom.document.location.host}${dom.document.location.pathname}ws"
  }

  def main(args: Array[String]): Unit = {
    dom.document.body = body("loading data …").render
    val registry = new Registry
    val connection: Future[RemoteRef] = registry.connect(WS(wsUri))
    connection.foreach { remote =>
      val descriptionsCRDT = registry.lookup(Bindings.crdtDescriptions, remote)
      println(s"requesting $descriptionsCRDT")
      descriptionsCRDT.onComplete(t => println(s"got $t"))
      descriptionsCRDT.foreach { res =>
        val descriptions = res.valueSignal

        val manualStates = Evt[AppState]()

        val actions = new Actions(manualStates = manualStates)
        val index = new Index(actions, descriptions)
        val app = new ReaderApp()

        app.makeBody(index, manualStates).asFrag.applyTo(dom.document.body.parentElement)
      }
    }
  }

}
