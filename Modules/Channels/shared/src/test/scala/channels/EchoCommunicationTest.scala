package channels

import de.rmgk.delay.{Async, Callback}

import java.io.IOException
import java.net.{InetAddress, InetSocketAddress, ServerSocket, SocketException}
import java.nio.channels.ClosedChannelException
import java.util.concurrent.{Executors, Semaphore}
import scala.concurrent.ExecutionContext
import scala.util.{Failure, Success}

def printErrors[T](cb: T => Unit): Callback[T] =
  case Success(mb) => cb(mb)
  case Failure(ex) => ex match
      case ex: IOException if ex.getCause.isInstanceOf[InterruptedException] =>
      case ex: ClosedChannelException                                        =>
      case ex                                                                => ex.printStackTrace()

trait EchoCommunicationTest[Info](
    serverConn: ExecutionContext => (Info, LatentConnection[MessageBuffer]),
    clientConn: ExecutionContext => Info => LatentConnection[MessageBuffer]
) extends munit.FunSuite {

  // need an execution context that generates new tasks as TCP does lots of blocking
  val executor             = Executors.newCachedThreadPool()
  val ec: ExecutionContext = ExecutionContext.fromExecutor(executor)

  override def afterAll(): Unit = {
    executor.shutdownNow()
  }

  test("sample communication") {

    given abort: Abort = Abort()

    val toSend                 = List("Hi", "ho", "let’s", "go")
    val messageCounter         = Semaphore(0)
    var received: List[String] = Nil

    var traced = List.empty[String]

    def trace(msg: String) = synchronized {
      traced = msg :: traced
    }

    trace(s"test starting")

    val (info, serverLatent) = serverConn(ec)

    val echoServer: Prod[Connection[MessageBuffer]] =
      serverLatent.prepare: conn =>
        printErrors: mb =>
          trace(s"server received; echoing")
          conn.send(mb).runToFuture

    val client: Prod[Connection[MessageBuffer]] =
      clientConn.apply(ec).apply(info).prepare: conn =>
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
      trace(s"starting sending")
      val conn = client.bind
      trace(s"sending")
      ec.execute: () =>
        toSend.foreach: msg =>
          conn.send(ArrayMessageBuffer(msg.getBytes())).run:
            printErrors(_ => ())

    sending.run:
      printErrors: conn =>
        ()

    trace(s"test waiting")

    messageCounter.acquire(toSend.size)
    assertEquals(toSend.sorted, received.sorted, traced)
  }

}
