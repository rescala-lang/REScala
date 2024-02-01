package channel

import channel.udp.{Ctx, Prod}
import channel.{ArrayMessageBuffer, Bidirectional}
import de.rmgk.delay.Async
import munit.internal.io.PlatformIO.Files

import java.net.InetSocketAddress
import java.nio.file.Path
import java.text.Bidi
import java.util.concurrent.TimeUnit
import scala.concurrent.Await
import scala.concurrent.duration.{Duration, SECONDS}
import scala.util.{Failure, Success}

object EchoServerTestUDP {
  def main(args: Array[String]): Unit = {
    val port = 54468

    val listener = channel.udp.UdpInChan(port, Duration(1, SECONDS))
    val sender   = channel.udp.UDPOutChan(InetSocketAddress("localhost", port))

    def fork: Async[Any, Unit] = Async.fromCallback:
      val t = new Thread(() =>
        Async.handler.succeed(())
      )
      t.setDaemon(false)
      println(s"starting thread")
      t.start()

    val echoServer: Prod[Unit] = Async[Ctx]:
      fork.bind
      println(s"serving")
      val msg = listener.receive.bind
      val str = new String(msg.asArray)
      if str.startsWith("X")
      then println(s"received length: ${str.length}")
      else println(s"received msg: ${new String(msg.asArray)}")

    val client: Async[Any, Unit] = Async:
      sender.send(ArrayMessageBuffer("hello world!".getBytes)).bind
      sender.send(ArrayMessageBuffer(("X" * 65000).getBytes())).bind

    given ctx: Ctx = udp.Ctx()

    echoServer.run: res =>
      println(s"echo res: $res")
    client.run: res =>
      println(s"client res: $res")

    Thread.sleep(1000)
    println(s"done sleeping!")

    ctx.closeRequest = true

  }
}
