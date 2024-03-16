package channel

import channel.jettywebsockets.{JettyWsConnection, JettyWsListener}
import channel.{ArrayMessageBuffer, Ctx}
import de.rmgk.delay.Async
import org.eclipse.jetty.http.pathmap.PathSpec
import org.eclipse.jetty.server.handler.ContextHandler
import org.eclipse.jetty.server.{Server, ServerConnector}
import org.eclipse.jetty.websocket.server.WebSocketUpgradeHandler

import java.net.URI

class JettyTest extends munit.FunSuite {

  val port = 45851

  test("basic") {

    val listening = JettyWsListener.startListen(port, PathSpec.from("/registry/*"))

    // echo server
    val echoServer = Async[Ctx] {
      val connection = listening.connections.bind
      println(s"connection received")
      val messageBuffer = connection.receive.bind
      println(s"received ${messageBuffer.asArray.length}bytes")
      connection.send(messageBuffer).bind
    }.run(using Ctx())(println)

    listening.server.start()

    println(s"server starting …")

    Thread.sleep(100)

    val connect = Async[Ctx] {
      val outgoing = JettyWsConnection.connect(URI.create(s"ws://localhost:$port/registry/")).bind
      outgoing.send(ArrayMessageBuffer("hello world".getBytes)).bind
      println(s"send successfull")
      val answer = outgoing.receive.bind
      println(new String(answer.asArray))
    }.run(using Ctx()) { res =>
      println(s"stopping!")
      println(res)
      listening.server.stop()
    }

    listening.server.join()

    println(s"terminating")

  }

}
