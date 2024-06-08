package channel

import channel.jettywebsockets.{JettyWsConnection, JettyWsListener}
import de.rmgk.delay.{Async, Callback}

import java.nio.channels.ClosedChannelException
import java.util.concurrent.{CountDownLatch, Executors, Semaphore}
import scala.concurrent.ExecutionContext
import scala.util.{Failure, Success}
import channel.tcp.TCP
import channel.udp.UDP
import org.eclipse.jetty.http.pathmap.PathSpec

import java.net.{InetSocketAddress, URI}

class EchoServerTestTCP extends EchoCommunicationTest(
      TCP.listen("0", 54467, ChannelTestThreadPools.ec),
      TCP.connect("localhost", 54467, ChannelTestThreadPools.ec)
    )

class EchoServerTestUDP extends EchoCommunicationTest(
      UDP.sendreceive(InetSocketAddress("localhost", 54469), 54468, ChannelTestThreadPools.ec),
      UDP.sendreceive(InetSocketAddress("localhost", 54468), 54469, ChannelTestThreadPools.ec)
    )

class EchoServerTestJetty extends EchoCommunicationTest(
      (incoming: Incoming) =>
        Async[Abort] {
          val listener   = JettyWsListener.prepareServer(54470)
          val echoServer = listener.listen(PathSpec.from("/registry/*"))
          listener.server.start()
          echoServer.prepare(incoming).bind
        },
      JettyWsConnection.connect(URI.create(s"ws://localhost:54470/registry/"))
    )

object ChannelTestThreadPools {
  // need an execution context that generates new tasks as TCP does lots of blocking
  val executor             = Executors.newCachedThreadPool()
  val ec: ExecutionContext = ExecutionContext.fromExecutor(executor)
}

def printErrors[T](cb: T => Unit): Callback[T] =
  case Success(mb) => cb(mb)
  case Failure(ex) => ex match
      case ex: ClosedChannelException =>
      case ex                         => ex.printStackTrace()

trait EchoCommunicationTest(serverConn: LatentConnection, clientConn: LatentConnection) extends munit.FunSuite {

  test("sample communication") {

    given abort: Abort = Abort()

    val toSend                 = List("Hi", "ho", "let’s", "go")
    val messageCounter         = Semaphore(0)
    var received: List[String] = Nil

    var traced = List.empty[String]

    def trace(msg: String) =
      synchronized { traced = msg :: traced }

    trace(s"test starting")

    val echoServer: Prod[ConnectionContext] =
      serverConn.prepare: conn =>
        printErrors: mb =>
          trace(s"server received; echoing")
          conn.send(mb).runToFuture

    val client: Prod[ConnectionContext] =
      clientConn.prepare: conn =>
        printErrors: mb =>
          trace(s"client received")
          synchronized {
            received = String(mb.asArray) :: received
          }
          messageCounter.release()

    echoServer.run:
      printErrors: conn =>
        ()

    val sending = Async: (_: Abort) ?=>
      val conn = client.bind
      trace(s"sending")
      ChannelTestThreadPools.ec.execute: () =>
        toSend.foreach: msg =>
          conn.send(ArrayMessageBuffer(msg.getBytes())).run:
            printErrors(_ => ())

    sending.run:
      printErrors: conn =>
        ()

    trace(s"test waiting")

    messageCounter.acquire(toSend.size)
    assertEquals(toSend.sorted, received.sorted)
  }

}
