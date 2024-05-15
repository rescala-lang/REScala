package channel

import channel.jettywebsockets.JettyWsConnection
import channel.{ArrayMessageBuffer, Abort}
import de.rmgk.delay.*

import java.net.URI

object JettyConnectTest {

  val port = 8080

  def main(args: Array[String]): Unit = {
    val connect = Async[Abort] {
      val outgoing = JettyWsConnection.connect(URI.create(s"ws://localhost:$port/registry/")).bind
      outgoing.send(ArrayMessageBuffer("hello world".getBytes)).bind
      println(s"send successfull")
      val answer = outgoing.receive.bind
      println(new String(answer.asArray))
    }.run(using Abort()) { res =>
      println(s"stopping!")
      println(res)
    }
  }

}
