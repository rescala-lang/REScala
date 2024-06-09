package channels

import channels.webnativewebsockets.WebsocketConnect
import de.rmgk.delay.{Async, Callback}

import scala.util.{Failure, Success}

def printErrors[T](cb: T => Unit): Callback[T] =
  case Success(mb) => cb(mb)
  case Failure(ex) => ex.printStackTrace()

class EchoWSTest extends munit.FunSuite {

  test("echo") {

    val outgoing = WebsocketConnect.connect("wss://echo.websocket.org/.ws").prepare: conn =>
      printErrors { msg =>
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
