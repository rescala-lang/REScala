package rescala.debuggable

import scala.scalajs.js
import scala.scalajs.js.Dynamic.literal
import scala.scalajs.js.JSON

object ChromeDebuggerInterface extends DebuggerInterface {

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
    println(JSON.stringify(data))
    org.scalajs.dom.window.postMessage(data, "*")
  }

}
