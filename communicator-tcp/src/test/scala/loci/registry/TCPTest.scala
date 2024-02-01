package loci.registry

import de.rmgk.delay.Async
import loci.communicator.tcp.ArrayMessageBuffer

import java.util.concurrent.TimeUnit
import scala.concurrent.Await
import scala.concurrent.duration.{Duration, SECONDS}

object EchoServerTest {
  def main(args: Array[String]): Unit = {
    val port = 54467

    val listener = loci.communicator.tcp.startListening(port, "0")

    def newThread: Async[Any, Unit] = Async.fromCallback:
      new Thread(() =>
        Async.handler.succeed(())
      ).start()

    val echoServer: Async[Any, Unit] = Async:
      newThread.bind
      println(s"serving")
      val channel = listener.channels.bind
      println(s"new connection")
      newThread.bind
      val msg = channel.in.receive.bind
      println(s"echoing")
      channel.out.send(msg).bind
      println(s"done")

    val client: Async[Any, Unit] = Async:
      newThread.bind
      println(s"connecting")
      val bidi = loci.communicator.tcp.connect("localhost", port).bind
      println(s"sending")
      bidi.out.send(ArrayMessageBuffer(Array(1, 2, 3, 4))).bind
      bidi.out.send(ArrayMessageBuffer(Array(5, 6, 7, 8))).bind
      println(s"receiving")
      val response = bidi.in.receive.bind
      println(response.asArray.mkString("[", ", ", "]"))

    echoServer.run: res =>
      println(s"echo res: $res")
    client.run: res =>
      println(s"client res: $res")

    Thread.sleep(10000)
  }
}

class TCPTest extends munit.FunSuite {

  test("basic") {}

}
