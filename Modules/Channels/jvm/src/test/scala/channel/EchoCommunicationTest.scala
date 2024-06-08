package channel

import de.rmgk.delay.{Async, Callback}

import java.util.concurrent.Executors
import scala.concurrent.ExecutionContext
import scala.util.{Failure, Success}

object ChannelTestThreadPools {
  // need an execution context that generates new tasks as TCP does lots of blocking
  val executor             = Executors.newCachedThreadPool()
  val ec: ExecutionContext = ExecutionContext.fromExecutor(executor)
}

def printErrors[T](cb: T => Unit): Callback[T] =
  case Success(mb) => cb(mb)
  case Failure(ex) => ex.printStackTrace()

trait EchoCommunicationTest(serverConn: LatentConnection, clientConn: LatentConnection) extends munit.FunSuite {

  test("sample communication") {
    given abort: Abort = Abort()

    val echoServer: Prod[ConnectionContext] =
      serverConn.prepare: conn =>
        printErrors: mb =>
          println(s"echoing")
          conn.send(mb).runToFuture

    val client: Prod[ConnectionContext] =
      clientConn.prepare: conn =>
        printErrors: mb =>
          println(s"received")
          println(mb.asArray.mkString("[", ", ", "]"))

    echoServer.run:
      printErrors: conn =>
        ()

    val sending = Async: (_: Abort) ?=>
      val conn = client.bind
      println(s"sending")
      conn.send(ArrayMessageBuffer(Array(1, 2, 3, 4))).bind
      println(s"sending")
      conn.send(ArrayMessageBuffer(Array(5, 6, 7, 8))).bind
      Thread.sleep(1000)
      println(s"done sleeping!")
      abort.closeRequest = true
      conn.close()

    sending.run:
      printErrors: conn =>
        ()

  }

}
