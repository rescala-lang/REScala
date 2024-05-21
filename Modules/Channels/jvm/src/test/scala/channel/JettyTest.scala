package channel

import channel.jettywebsockets.{JettyWsConnection, JettyWsListener}
import channel.{ArrayMessageBuffer, Abort}
import de.rmgk.delay.Async
import org.eclipse.jetty.http.pathmap.PathSpec
import org.eclipse.jetty.server.handler.ContextHandler
import org.eclipse.jetty.server.{Server, ServerConnector}
import org.eclipse.jetty.websocket.server.WebSocketUpgradeHandler

import java.net.URI

class JettyTest extends munit.FunSuite {

  def println(str: Any): Unit = System.out.println(s"$str [${Thread.currentThread().getName()}]")

  val port = 45851

  test("basic") {

    val listener = JettyWsListener.prepareServer(port)
    val echoServer = listener.listen(PathSpec.from("/registry/*")).prepare: conn =>
      println(s"server connection received")
      printErrors: messageBuffer =>
        println(s"received ${messageBuffer.asArray.length}bytes")
        conn.send(messageBuffer).run(printErrors(_ => println(s"answered successfully")))

    echoServer.run(using Abort())(printErrors(_ => ()))

    println(s"server starting …")
    listener.server.start()

    val connector = JettyWsConnection.connect(URI.create(s"ws://localhost:$port/registry/")).prepare: conn =>
      println(s"client connected")
      printErrors: msg =>
        println(s"received: ${new String(msg.asArray)}")

    val connect = Async[Abort] {
      // this shifts execution to the websocket client thread (and there seems to be only one for this connection)
      val outgoing = connector.bind
      println(s"sending message")
      // keeps execution on the websocket client thread
      outgoing.send(ArrayMessageBuffer("hello world??".getBytes)).bind
      println(s"send successfull")
    }.run(using Abort()) { res =>
      // this is still running on the websocket client thread and blocking execution the client handler
      println(s"stopping!")
      println(res)
      listener.server.stop()
    }

    println(s"joining server …")
    listener.server.join()

    println(s"terminating")

  }

}
