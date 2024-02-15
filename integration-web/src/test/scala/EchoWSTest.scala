package channel

import channel.webnative.WebsocketConnect
import de.rmgk.delay.Async

class EchoWSTest extends munit.FunSuite {

  test("echo") {
    val fut = Async[Ctx]:
      val wsc = WebsocketConnect.connect("wss://echo.websocket.org/.ws").bind
      println(s"connected")
      wsc.send(ArrayMessageBuffer("hello world".getBytes())).bind
      val buf = wsc.receive.bind
      println(s"received: ${new String(buf.asArray)}")
    .runToFuture(using Ctx())
    fut
  }

}
