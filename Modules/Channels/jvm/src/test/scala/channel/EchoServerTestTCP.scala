package channel

import channel.tcp.TCP
import de.rmgk.delay.syntax.run
import de.rmgk.delay.{Async, Callback}

import java.util.concurrent.Executors
import scala.concurrent.ExecutionContext
import scala.util.{Failure, Success}

def printErrors[T](cb: T => Unit): Callback[T] =
  case Success(mb) => cb(mb)
  case Failure(ex) => ex.printStackTrace()

object EchoServerTestTCP {
  def main(args: Array[String]): Unit = {
    val port = 54467

    given abort: Abort = Abort()

    // need an execution context that generates new tasks as TCP does lots of blocking
    val executor             = Executors.newCachedThreadPool()
    val ec: ExecutionContext = ExecutionContext.fromExecutor(executor)

    val echoServer: Prod[ConnectionContext] =
      TCP.listen("0", port, ec).prepare: conn =>
        printErrors: mb =>
          println(s"echoing")
          conn.send(mb).runToFuture

    val client: Prod[ConnectionContext] =
      TCP.connect("localhost", port, ec).prepare: conn =>
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
      executor.shutdownNow()

    sending.run:
      printErrors: conn =>
        ()

  }
}
