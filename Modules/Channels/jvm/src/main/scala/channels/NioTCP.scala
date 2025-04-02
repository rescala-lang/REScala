package channels

import de.rmgk.delay
import de.rmgk.delay.{Async, Callback, Sync}

import java.io.IOException
import java.net.{SocketAddress, SocketException, StandardProtocolFamily, StandardSocketOptions, UnixDomainSocketAddress}
import java.nio.ByteBuffer
import java.nio.channels.{SelectionKey, Selector, ServerSocketChannel, SocketChannel}
import scala.annotation.tailrec
import scala.util.control.NonFatal
import scala.util.{Failure, Success}

case class AcceptAttachment(
    callback: Callback[Connection[MessageBuffer]],
    incoming: Receive[MessageBuffer],
)

case class ReceiveAttachment(
    callback: Callback[MessageBuffer]
)

/** [[loopSelection]] and [[runSelection]] should not be called from multiple threads at the same time.
  * Only one thread should send on a single connection at the same time.
  */
class NioTCP {

  val selector: Selector = Selector.open()

  def loopSelection(abort: Abort) = {
    while !abort.closeRequest do
      selector.select()
      runSelection()
  }

  def runSelection() = {

    selector.selectedKeys().forEach {
      case key if key.isReadable =>

        val clientChannel = key.channel().asInstanceOf[SocketChannel]
        val attachment    = key.attachment().asInstanceOf[ReceiveAttachment]
        try {

          val len          = readN(4, clientChannel).getInt()
          val bytes        = new Array[Byte](len)
          val targetBuffer = readN(len, clientChannel).get(bytes)

          attachment.callback.succeed(ArrayMessageBuffer(bytes))
        } catch {
          case ex: IOException =>
            clientChannel.close()
            key.cancel()
            attachment.callback.fail(ex)
        }

      case key if key.isAcceptable =>

        val serverChannel = key.channel().asInstanceOf[ServerSocketChannel]

        val attachment = key.attachment().asInstanceOf[AcceptAttachment]

        val clientChannel = serverChannel.accept()
        try {
          attachment.callback.succeed {
            handleConnection(clientChannel, attachment.incoming)
          }
        } catch {
          case exception: SocketException =>
            attachment.callback.fail(exception)
        }

    }

    selector.selectedKeys().clear()
  }

  class NioTCPConnection(clientChannel: SocketChannel) extends Connection[MessageBuffer] {

    override def send(message: MessageBuffer): Async[Any, Unit] = Sync {

      val bytes         = message.asArray
      val messageLength = bytes.length

      val buffer = ByteBuffer.wrap(bytes)

      val sizeBuffer = ByteBuffer.allocate(4)
      sizeBuffer.putInt(messageLength)
      sizeBuffer.flip()

      val buffers = Array(sizeBuffer, buffer)

      while buffer.hasRemaining() do {
        val res = clientChannel.write(buffers)
        ()
      }
      ()

    }
    override def close(): Unit = clientChannel.close()
  }

  def handleConnection(
      clientChannel: SocketChannel,
      incoming: Receive[MessageBuffer],
  ): NioTCPConnection = {

    configureChannel(clientChannel)

    val conn = NioTCPConnection(clientChannel)

    val callback = incoming.messageHandler(conn)
    clientChannel.register(selector, SelectionKey.OP_READ, ReceiveAttachment(callback))
    selector.wakeup()

    conn
  }

  def readN(n: Int, clientChannel: SocketChannel): ByteBuffer = {
    val buffer    = ByteBuffer.allocate(n)
    var bytesRead = 0
    while bytesRead < n do {
      val result = clientChannel.read(buffer)
      if result == -1 then {
        throw IOException("nothing read???")
      }
      bytesRead += result
    }
    buffer.flip()
    buffer
  }

  def connect(
      bindsocket: () => SocketChannel,
  ): LatentConnection[MessageBuffer] =
    connectRetrying(bindsocket, 1000, 0)

  def connectRetrying(
      bindsocket: () => SocketChannel,
      delay: Long,
      tries: Int
  ): LatentConnection[MessageBuffer] =
    new LatentConnection {
      override def prepare(incoming: Receive[MessageBuffer]): Async[Abort, Connection[MessageBuffer]] =
        Async.fromCallback {
          @tailrec
          def retry(delay: Long, tries: Int): Unit = {
            try
              Async.handler.succeed {
                handleConnection(bindsocket(), incoming)
              }
            catch
              case NonFatal(exception) =>
                if tries == 0
                then Async.handler.fail(exception)
                else
                  println(s"Failed to connect to socket, retrying in ${delay}ms")
                  Thread.sleep(delay)
                  retry(delay, tries - 1)
          }
          retry(delay, tries)
        }
    }

  def defaultSocketChannel(socketAddress: SocketAddress): () => SocketChannel = () => {
    val pf = socketAddress match
      case _: UnixDomainSocketAddress => StandardProtocolFamily.UNIX
      case other                      => StandardProtocolFamily.INET
    val channel = SocketChannel.open(pf)
    channel.connect(socketAddress)
    configureChannel(channel)
    channel
  }

  private def configureChannel(channel: SocketChannel) = {
    channel.configureBlocking(false)
    try
      channel.setOption(StandardSocketOptions.TCP_NODELAY, true)
    catch
      case _: UnsupportedOperationException =>
        println(s"TCP nodelay not supported on this socket")
    // channel.setOption(StandardSocketOptions.SO_REUSEADDR, true)
    // channel.setOption(StandardSocketOptions.SO_RCVBUF, tcpBufferSizes)
    // channel.setOption(StandardSocketOptions.SO_SNDBUF, tcpBufferSizes)
  }

  def defaultServerSocketChannel(socketAddress: SocketAddress): () => ServerSocketChannel = () => {
    val pf = socketAddress match
      case _: UnixDomainSocketAddress => StandardProtocolFamily.UNIX
      case other                      => StandardProtocolFamily.INET
    val socket = ServerSocketChannel.open(pf)
    socket.configureBlocking(false)

    socket.bind(socketAddress)
    socket
  }

  def listen(
      bindsocket: () => ServerSocketChannel,
  ): LatentConnection[MessageBuffer] =
    new LatentConnection {
      override def prepare(incoming: Receive[MessageBuffer]): Async[Abort, Connection[MessageBuffer]] =
        Async.fromCallback { abort ?=>
          try {
            val serverChannel: ServerSocketChannel = bindsocket()

            val callback = Async.handler[Connection[MessageBuffer]]
            serverChannel.register(selector, SelectionKey.OP_ACCEPT, AcceptAttachment(callback, incoming))
            selector.wakeup()
            ()
          } catch
            case NonFatal(ex) => Async.handler.fail(ex)

        }
    }
}
