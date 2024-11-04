package channels

import de.rmgk.delay
import de.rmgk.delay.{Async, Callback}

import java.io.IOException
import java.net.{SocketAddress, SocketException, StandardProtocolFamily, UnixDomainSocketAddress}
import java.nio.ByteBuffer
import java.nio.channels.{SelectionKey, Selector, ServerSocketChannel, SocketChannel}
import scala.concurrent.ExecutionContext
import scala.util.control.NonFatal
import scala.util.{Failure, Success}

object NioTCP {

  class NioTCPConnection(clientChannel: SocketChannel) extends Connection[MessageBuffer] {

    override def send(message: MessageBuffer): Async[Any, Unit] = Async {

      val bytes         = message.asArray
      val messageLength = bytes.length

      val buffer = ByteBuffer.allocate(4 + bytes.length)
      buffer.putInt(messageLength)
      buffer.put(bytes)
      buffer.flip()

      NioTCP.synchronized {
        while buffer.hasRemaining() do {
          val res = clientChannel.write(buffer)
          ()
        }
      }
      ()

    }
    override def close(): Unit = clientChannel.close()
  }

  def handleConnection(
      clientChannel: SocketChannel,
      incoming: Handler[MessageBuffer],
      executionContext: ExecutionContext,
      abort: Abort
  ): NioTCPConnection = {
    println(s"handling new connection")

    val selector = Selector.open()
    clientChannel.configureBlocking(false)
    clientChannel.register(selector, SelectionKey.OP_READ)

    val conn = NioTCPConnection(clientChannel)

    val callback = incoming.getCallbackFor(conn)

    executionContext.execute { () =>
      println(s"executing task")
      try {
        while !abort.closeRequest do {
          selector.select()

          selector.selectedKeys().forEach { key =>
            if key.isReadable then {

              try {

                val len          = readN(4, clientChannel).getInt()
                val bytes        = new Array[Byte](len)
                val targetBuffer = readN(len, clientChannel).get(bytes)

                callback.succeed(ArrayMessageBuffer(bytes))
              } catch
                case ex: IOException =>
                  clientChannel.close()
                  key.cancel()
                  callback.fail(ex)
            } else {
              throw IllegalStateException("why am I here? again")
            }

          }

          selector.selectedKeys().clear()
        }
      } catch
        case exception: SocketException =>
          callback.fail(exception)
    }

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
      socketAddress: SocketAddress,
      executionContext: ExecutionContext,
      abort: Abort
  ): LatentConnection[MessageBuffer] =
    def socketChannel: SocketChannel = {
      val pf = socketAddress match
        case _: UnixDomainSocketAddress => StandardProtocolFamily.UNIX
        case other                      => StandardProtocolFamily.INET
      val channel = SocketChannel.open(pf)
      channel.connect(socketAddress)
      channel.configureBlocking(false)
      channel
    }
    connect(() => socketChannel, executionContext, abort)

  def connect(
      bindsocket: () => SocketChannel,
      executionContext: ExecutionContext,
      abort: Abort
  ): LatentConnection[MessageBuffer] =
    new LatentConnection {
      override def prepare(incoming: Handler[MessageBuffer]): Async[Any, Connection[MessageBuffer]] =
        TCP.syncAttempt {
          println(s"tcp sync attempt")
          handleConnection(bindsocket(), incoming, executionContext, abort)
        }
    }

  def defaultSocket(socketAddress: SocketAddress): () => ServerSocketChannel = () => {
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
      executionContext: ExecutionContext
  ): LatentConnection[MessageBuffer] =
    new LatentConnection {
      override def prepare(incoming: Handler[MessageBuffer]): Async[Abort, Connection[MessageBuffer]] =
        Async.fromCallback { abort ?=>
          try {
            val serverChannel: ServerSocketChannel = bindsocket()

            val selector = Selector.open()
            serverChannel.register(selector, SelectionKey.OP_ACCEPT)

            executionContext.execute { () =>
              try {
                while !abort.closeRequest do {
                  selector.select()

                  selector.selectedKeys().forEach { key =>
                    if key.isAcceptable then {

                      val clientChannel = serverChannel.accept()

                      Async.handler.succeed {
                        handleConnection(clientChannel, incoming, executionContext, abort)
                      }
                    } else {
                      throw IllegalStateException("why am I here?")
                    }

                  }

                  selector.selectedKeys().clear()
                }
              } catch {
                case exception: SocketException =>
                  Async.handler.fail(exception)
              }
            }
          } catch
            case NonFatal(ex) => Async.handler.fail(ex)

        }
    }
}
