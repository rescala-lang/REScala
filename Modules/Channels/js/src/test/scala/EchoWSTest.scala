package channel

import channel.webnativewebsockets.WebsocketConnect
import de.rmgk.delay.Async

class EchoWSTest extends munit.FunSuite {

  test("echo") {
    val fut = Async[Abort]:
      val wsc = WebsocketConnect.connect("wss://echo.websocket.org/.ws").bind
      println(s"connected")
      wsc.send(ArrayMessageBuffer("hello world".getBytes())).bind
      val buf = wsc.receive.bind
      println(s"received: ${new String(buf.asArray)}")
    .runToFuture(using Abort())
    fut
  }

}
