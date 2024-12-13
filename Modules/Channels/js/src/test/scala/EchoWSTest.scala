package channels

import channels.webnativewebsockets.WebsocketConnect
import de.rmgk.delay.{Async, Callback}

import scala.util.{Failure, Success}

class EchoWSTest extends munit.FunSuite {

  test("echo") {

    val outgoing = WebsocketConnect.connect("wss://echo.websocket.org/.ws").prepare: conn =>
      TestUtil.printErrors { msg =>
        println(s"received: ${new String(msg.asArray)}")
      }

    val fut = Async[Abort]:
      val wsc = outgoing.bind
      println(s"connected")
      wsc.send(ArrayMessageBuffer("hello world".getBytes())).bind
    .runToFuture(using Abort())
    fut
  }

}
