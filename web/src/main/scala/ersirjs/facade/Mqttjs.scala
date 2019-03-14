package ersirjs.facade

import org.scalajs.dom
import rescala.default._

import scala.collection.mutable
import scala.scalajs.js
import scala.scalajs.js.annotation.JSImport


@JSImport("mqtt", JSImport.Namespace)
@js.native
object Mqttjs extends js.Object {
  def connect(i: String): js.Dynamic = js.native
}

object ReMqtt {
  var connection: js.Dynamic = _
  def isConnected(): Boolean = js.DynamicImplicits.truthValue(connection.connected)
  val connected                                = Var[Boolean](false)
  val topics: mutable.Map[String, Evt[String]] = mutable.Map[String, Evt[String]]()

  val wsUri: String = {
    //    val wsProtocol = if (dom.document.location.protocol == "https:") "wss" else "ws"
    val wsProtocol = "ws"
    val hash = dom.window.location.hash
    if (hash.length > 2) hash.substring(1)
    else s"$wsProtocol://${dom.document.location.host}:9001/"

  }

  def start() = {

    println(s"initializing mqtt $wsUri …")


    connection = Mqttjs.connect(wsUri)

    connected.observe(c => println(s"connected => $c"))

    connection.on("connect", () => {
      println("connect event")
      connected.set(isConnected)
    })
    connection.on("reconnect", () => {
      println("reconnect event")
      connected.set(isConnected)
    })
    connection.on("close", () => {
      println("close event")
      connected.set(isConnected)
    })
    connection.on("error", () => {
      println("error event")
      connected.set(isConnected)
    })

    connection.on("message", { (topic: String, message: js.Dynamic) =>
      topics.get(topic).foreach(e => e.fire(message.toString))
    })
    println(s"mqtt initialized")
  }

  def send(channel: String, content: String): Unit = {
    connection.publish(channel, content)
  }


  def topicstream(topic: String): Evt[String] = {
    topics.getOrElseUpdate(topic, {
      println(s"subscribing to mqtt topic $topic")
      connection.subscribe(topic)
      println(s"subscribed to $topic")
      Evt[String]
    })
  }
}
