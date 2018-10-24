package rescala.debuggable

import scala.scalajs.js
import scala.scalajs.js.Dynamic.literal
import scala.scalajs.js.JSON

object ChromeDebuggerInterface extends DebuggerInterface {

//  val map = mutable.Map[String, ]()

  def saveNode(id: NodeID, name: String, value: String): Unit = {
    val msg = literal(
      content = literal(
        nodeId = id.str,
//      'nodeType': type,
//      'nodeMethod': method,
        nodeRef = name,
        nodeValue = value,
//        sourceInfo = sourceInfo,
        ),
      action = "saveNode",
      destination = "panel"
    )
    send(msg)
  }

  def saveEdge(from: NodeID, to: NodeID): Unit = {
    val msg = literal(
      content = literal(
        edgeStart = from.str,
//        "edgeStartName": edgeStart ? edgeStart.name : '',
        edgeEnd = to.str,
//  "edgeEndName": edgeEnd ? edgeEnd.name : '',
//  "edgeLabel": name
        ),
      action = "saveEdge",
      destination = "panel"
    )
    send(msg)
  }

  def send(data: js.Object) = {
    if (!isSetup) setup()
    println("send " + JSON.stringify(data))
    org.scalajs.dom.window.postMessage(data, "*")
  }

  var isSetup = false
  def setup(): Unit = {
    isSetup = true
    org.scalajs.dom.window.onmessage = {e: org.scalajs.dom.MessageEvent =>
      val data = e.data.asInstanceOf[js.Dynamic]
      println(("will rescala get? 3"))
      if (data.destination.asInstanceOf[String] == "rescala") {
        if (data.type == "set-signal") {
          val nodeId = data.nodeId.asInstanceOf[String]
          val value = data.value.asInstanceOf[String]
          // TODO set nodeId to value
          println(JSON.stringify(nodeId) + " fire " + JSON.stringify(value))
        }
      } else {
      }
      println(JSON.stringify(data))
//      if (data.destination.asInstanceOf[String] == "rescala")
    }
  }

}
