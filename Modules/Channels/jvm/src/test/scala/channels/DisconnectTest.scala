package channels

import java.net.StandardProtocolFamily
import java.nio.channels.{ServerSocketChannel, SocketChannel}
import scala.concurrent.ExecutionContext
import scala.util.{Failure, Success}

class DisconnectTest extends munit.FunSuite {

  lazy val isMacOS: Boolean = sys.props.get("os.name").exists(_.toLowerCase.contains("mac"))
  override def munitIgnore: Boolean = isMacOS

  test("disconnect nioTCP") {

    val ec = ExecutionContext.global

    val socket = ServerSocketChannel.open(StandardProtocolFamily.UNIX)

    socket.configureBlocking(false)

    // socket.bind(new InetSocketAddress("localhost", 0))

    val socketPath = domainSocketHelperNonensese("some-name-disconnect")

    socket.bind(socketPath)

    val serverNioTCP = new NioTCP

    val serverAbort = Abort()

    ec.execute(() => serverNioTCP.loopSelection(serverAbort))

    val listen = serverNioTCP.listen(() => socket)

    listen.prepare(conn =>
      TestUtil.printErrors { mb =>
        println(s"server pong received: ${new String(mb.asArray)}")
        conn.send(mb).run(using ())(TestUtil.printErrors(mb => ()))
      }
    ).run(using Abort()) {
      case Success(_) => println("connection successful")
      case Failure(_) => println("connection failed")
    }

    def socketChannel: SocketChannel = {
      val channel = SocketChannel.open(StandardProtocolFamily.UNIX)
      channel.connect(socketPath)
      channel.configureBlocking(false)
      channel
    }

    val clientNioTCP = new NioTCP
    ec.execute(() => clientNioTCP.loopSelection(Abort()))
    val connect = serverNioTCP.connect(() => socketChannel)

    connect.prepare { conn =>
      {
        case Success(mb) => println(s"client pong received: ${new String(mb.asArray)}")
        case Failure(ex) =>
          assertEquals(ex.getMessage, "nothing read???")
      }
    }.run(using Abort()) {
      case Success(conn) =>
        println(s"client connected successfully, sending")
        conn.send(ArrayMessageBuffer("Hi!".getBytes())).run(using Abort()) { TestUtil.printErrors(mb => ()) }
        Thread.sleep(10)
        serverNioTCP.selector.keys().forEach(_.channel().close())
        Thread.sleep(10)

        conn.send(ArrayMessageBuffer("Hi 2!".getBytes())).run(using Abort()) { TestUtil.printErrors(mb => ()) }
      case Failure(_) =>
    }

    assert(true)

  }
}
