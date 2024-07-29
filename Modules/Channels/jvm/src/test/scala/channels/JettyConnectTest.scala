package channels

import channels.Abort
import channels.jettywebsockets.JettyWsConnection
import de.rmgk.delay.*

import java.net.URI

object JettyConnectTest {

  def main(args: Array[String]): Unit = {

    val prepared = JettyWsConnection.connect(URI.create(s"wss://echo.websocket.org/")).prepare: conn =>
      println(s"established connection")
      msg =>
        println(s"received ${msg.map(_.show)}")

    val connect = Async[Abort] {
      val outgoing = prepared.bind
      outgoing.send(ArrayMessageBuffer("hello world".getBytes)).bind
      println(s"send successfull")
    }.run(using Abort()) { res =>
      println(s"done!")
      println(res)
    }
  }

}
