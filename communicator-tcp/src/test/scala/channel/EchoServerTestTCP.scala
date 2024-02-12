package channel

import channel.tcp.TCPConnection
import channel.{ArrayMessageBuffer, Bidirectional}
import de.rmgk.delay.Async

import java.text.Bidi
import java.util.concurrent.TimeUnit
import scala.concurrent.Await
import scala.concurrent.duration.{Duration, SECONDS}
import scala.util.{Failure, Success}
import de.rmgk.delay.syntax.run

object EchoServerTestTCP {
  def main(args: Array[String]): Unit = {
    val port = 54467

    val listener = channel.tcp.startListening(port, "0")

    def fork: Async[Any, Unit] = Async.fromCallback:
      val t = new Thread(() =>
        Async.handler.succeed(())
      )
      t.setDaemon(true)
      println(s"starting thread")
      t.start()

    val echoServer: Prod[Unit] = Async[Ctx]:
      fork.bind
      println(s"serving")
      val channel = listener.channels.bind
      println(s"new connection")
      fork.bind
      val msg = channel.receive.bind
      println(s"echoing")
      channel.send(msg).bind
      println(s"done")

    val client: Prod[TCPConnection] =
      val bidiA = Async:
        fork.bind
        println(s"connecting")
        channel.tcp.connect("localhost", port).bind
      .runToAsync

      val sending = Async: (_: Any) ?=>
        val bidi = bidiA.bind
        println(s"sending")
        fork.bind
        bidi.send(ArrayMessageBuffer(Array(1, 2, 3, 4))).bind
        bidi.send(ArrayMessageBuffer(Array(5, 6, 7, 8))).bind

      val receiving = Async[Ctx]:
        val bidi = bidiA.bind
        fork.bind
        println(s"receiving")
        val response = bidi.receive.bind
        println(response.asArray.mkString("[", ", ", "]"))

      Async[Ctx]:
        sending.bind
        receiving.bind
        bidiA.bind

    var bidi: TCPConnection = null

    echoServer.run(using Ctx()): res =>
      println(s"echo res: $res")
    client.run(using Ctx()):
      case Success(res) => bidi = res
      case Failure(e)   => throw e

    Thread.sleep(1000)

    println(s"done sleeping!")

    listener.close()
    bidi.close()

  }
}
