package channel

import channel.{ArrayMessageBuffer}
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

    val listener = channel.udp.UdpInChan.listen(port, Duration(1, SECONDS))
    val sender   = channel.udp.UDPOutChan.establish(InetSocketAddress("ip6-localhost", port))

    def fork: Async[Any, Unit] = Async.fromCallback:
      val t = new Thread(() =>
        Async.handler.succeed(())
      )
      t.setDaemon(false)
      println(s"starting thread")
      t.start()

    val echoServer: Prod[Unit] = Async[Abort]:
      fork.bind
      println(s"serving")
      val msg = listener.receive.bind
      val str = new String(msg.asArray)
      if str.startsWith("X")
      then println(s"received length: ${str.length}")
      else println(s"received msg: ${new String(msg.asArray)}")

    val client: Async[Any, Unit] = Async:
      sender.send(ArrayMessageBuffer("hello world!".getBytes)).bind
      // 65507 bytes seems to be the maximum for IPv4, 8 byte udp header 20 byte IP header, but what about the 4 byte of length we also sent?
      sender.send(ArrayMessageBuffer(("X" * ((1 << 16) - 29)).getBytes())).bind
      sender.send(ArrayMessageBuffer(("X" * ((1 << 16) - 28)).getBytes())).bind

      // ipv6 allows larger packets, and this here then seems to overflow the UDP header size, again, not sure where the 4 byte of length vanish to …
      sender.send(ArrayMessageBuffer(("X" * ((1 << 16) - 9)).getBytes())).bind
      sender.send(ArrayMessageBuffer(("X" * ((1 << 16) - 8)).getBytes())).bind

    given ctx: Abort = Abort()

    echoServer.run: res =>
      println(s"echo res: $res")
    client.run: res =>
      println(s"client res: $res")

    Thread.sleep(1000)
    println(s"done sleeping!")

    ctx.closeRequest = true

  }
}
