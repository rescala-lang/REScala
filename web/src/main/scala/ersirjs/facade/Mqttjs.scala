package ersirjs.facade

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
  println(s"initializing mqtt …")
  val connection: js.Dynamic = Mqttjs.connect("ws://127.0.0.1:9001")
  def isConnected(): Boolean = js.DynamicImplicits.truthValue(connection.connected)
  val connected = Var[Boolean](isConnected)
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

  val topics: mutable.Map[String, Evt[String]] = mutable.Map[String, Evt[String]]()
  connection.on("message", { (topic: String, message: js.Dynamic) =>
    topics.get(topic).foreach(e => e.fire(message.toString))
  })
  println(s"mqtt initialized")



  def topicstream(topic: String): Evt[String] = {
    topics.getOrElseUpdate(topic, {
      println(s"subscribing to mqtt topic $topic")
      connection.subscribe(topic)
      println(s"subscribed to $topic")
      Evt[String]
    })
  }
}
