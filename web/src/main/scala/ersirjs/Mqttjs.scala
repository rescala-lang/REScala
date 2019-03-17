package ersirjs

import org.scalajs.dom
import rescala.default._

import scala.collection.mutable
import scala.scalajs.js
import scala.scalajs.js.JSON
import scala.scalajs.js.annotation.JSImport


@JSImport("mqtt", JSImport.Namespace)
@js.native
object Mqttjs extends js.Object {
  def connect(url: String): js.Dynamic = js.native
  def connect(url: String, options: js.Object): js.Dynamic = js.native
}


object ReMqtt {

  var connection: js.Dynamic = _
  def isConnected(): Boolean = js.DynamicImplicits.truthValue(connection.connected)
  val connected                                = Var[Boolean](false)
  val topics: mutable.Map[String, Evt[String]] = mutable.Map[String, Evt[String]]()


  def start() = {

    val wsUri: String = {
      //    val wsProtocol = if (dom.document.location.protocol == "https:") "wss" else "ws"
      val wsProtocol = "ws"
      val hash = dom.window.location.hash
      if (hash.length > 2) hash.substring(1)
      else s"$wsProtocol://${dom.document.location.hostname}:9001/"
    }

    println(s"initializing mqtt $wsUri …")


    connection = Mqttjs.connect(wsUri, js.Dynamic.literal("reconnectPeriod" -> 10000,
                                                          "connectTimeout" -> 1000))

    connected.observe(c => println(s"connected => $c"))

    List("connect", "reconnect", "close", "error").foreach { ev =>
      connection.on(ev, (data: js.Any) => {
        println(s"mqtt $ev event: ${JSON.stringify(data)}")
        connected.set(isConnected)
      })
    }

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
