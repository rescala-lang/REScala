package ersirjs

import ersir.shared._
import ersirjs.facade.ReMqtt
import ersirjs.render.Index
import loci.communicator.ws.akka.WS
import loci.registry.Registry
import loci.transmitter.RemoteRef
import org.scalajs.dom
import rescala.default._
import rescala.rescalatags._
import scalatags.JsDom.attrs.cls
import scalatags.JsDom.implicits._

import scala.concurrent.Future
import scala.scalajs.concurrent.JSExecutionContext.Implicits.queue


object ErsirJS {

  val wsUri: String = {
    val wsProtocol = if (dom.document.location.protocol == "https:") "wss" else "ws"
    s"$wsProtocol://${dom.document.location.host}${dom.document.location.pathname}ws"
  }

  def main(args: Array[String]): Unit = {
    println("initing")

//    val cytoscapeContainer = div(style := "height: 100vh;").render
//
//    dom.document.body = body(cytoscapeContainer)
//
//    val cy = Cytoscape(literal(container = cytoscapeContainer,
//                              elements = js.Array(
//                                literal(data = literal(id = "a")),
//                                literal(data = literal(id = "b")),
//                                literal(data = literal(id = "ab", source = "a", target = "b")),
//                                ),
//                              style = js.Array(
//                                literal(
//                                  selector = "node",
//                                  style = literal(
//                                    "background-color" -> "#666",
//                                    "label" -> "data(id)")
//                                ),
//                                literal(
//                                  selector = "edge",
//                                  style = literal(
//                                    "width" -> 3,
//                                    "line-color" -> "#ccc",
//                                    "target-arrow-color" -> "#ccc",
//                                    "target-arrow-shape" -> "triangle"
//                                  )
//                                )
//                              ),
//                              layout = literal(
//                                name = "grid",
//                                rows = 1,
//                                )))
//    println(cy.nodes().length)
//
//    cy.add(literal(data = literal(id = "c")))
//    cy.layout(literal(
//      name = "grid",
//      rows = 1,
//      )).run()


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
        val entrySignal = entryCrdt.valueSignal

        val emergencies = ReMqtt.topicstream("city/alert_state")
        val currentEmergency = emergencies.latest("")

        entryCrdt.append(
          """Breaking News!
Lorem ipsum dolor sit amet, consectetur adipisicing elit, sed do eiusmod
tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam,
quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo
consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse
cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non
proident, sunt in culpa qui officia deserunt mollit anim id est laborum.""")

        val manualStates = Evt[AppState]()

        val actions = new Actions(manualStates = manualStates)
        val index = new Index(actions, entrySignal)
        val app = new ReaderApp()


        val bodySig = Signal {
          app.makeBody(index, manualStates).value.apply(cls := currentEmergency)
        }


        val bodyParent = dom.document.body.parentElement
        bodyParent.removeChild(dom.document.body)
        bodySig.asFrag.applyTo(bodyParent)

//        app.makeBody(index, manualStates).asFrag.applyTo(dom.document.body.parentElement)
      }
    }
  }

}
