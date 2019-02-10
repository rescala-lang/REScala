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
import scala.collection.mutable

import scala.concurrent.Future
import scala.scalajs.concurrent.JSExecutionContext.Implicits.queue
import scala.scalajs.js
import scala.scalajs.js.annotation.JSImport

@JSImport("mqtt", JSImport.Namespace)
@js.native
object mqtt extends js.Object {
  def connect(i: String): js.Dynamic = js.native
}

object ReMqtt {
  val connection: js.Dynamic = mqtt.connect("ws://127.0.0.1:9001")
  val topics: mutable.Map[String, Evt[String]] = mutable.Map[String, Evt[String]]()
  connection.on("message", { (topic: String, message: js.Dynamic) =>
    topics.get(topic).foreach(e => e.fire(message.toString))
  })

  def topicstream(topic: String): Evt[String] = {
    topics.getOrElseUpdate(topic, {
      val evt = Evt[String]
      connection.subscribe(topic)
      evt
    })
  }
}


object ErsirJS {

  val wsUri: String = {
    val wsProtocol = if (dom.document.location.protocol == "https:") "wss" else "ws"
    s"$wsProtocol://${dom.document.location.host}${dom.document.location.pathname}ws"
  }

  def main(args: Array[String]): Unit = {
    println("initing")
    dom.document.body = body("loading data …").render

    val emergencies = ReMqtt.topicstream("city/alert_state")

    val registry = new Registry
    val connection: Future[RemoteRef] = registry.connect(WS(wsUri))
    connection.foreach { remote =>
      val descriptionsCRDT = registry.lookup(Bindings.crdtDescriptions, remote)
      println(s"requesting $descriptionsCRDT")
      descriptionsCRDT.failed.foreach{ t =>
        t.printStackTrace()
      }
      descriptionsCRDT.foreach { res =>
        val descriptions = res.valueSignal

        emergencies.observe{ str =>
          res.append(str)
        }

        val manualStates = Evt[AppState]()

        val actions = new Actions(manualStates = manualStates)
        val index = new Index(actions, descriptions)
        val app = new ReaderApp()

        app.makeBody(index, manualStates).asFrag.applyTo(dom.document.body.parentElement)
      }
    }
  }

}
