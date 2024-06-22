package api2

import com.github.plokhotnyuk.jsoniter_scala.core.*

import java.net.InetSocketAddress
import java.nio.ByteBuffer
import java.nio.channels.*

trait RemoteGraphConnector {
  def write[T: JsonValueCodec](v: T): Unit

  def read[T: JsonValueCodec](onRead: T => Unit): Unit
}

class TCPClientConnector(hostname: String, port: Int) extends RemoteGraphConnector {
  protected val chan: AsynchronousSocketChannel = AsynchronousSocketChannel.open()

  def connect(): TCPClientConnector = {
    chan.connect(InetSocketAddress(hostname, port)).get()
    this
  }

  def connect(rg: RemoteGraph): TCPClientConnector = {
    chan.connect(InetSocketAddress(hostname, port)).get()
    rg.setConnector(this)
    this
  }

  def closeConnection(): Unit = chan.close()

  override def write[T: JsonValueCodec](v: T): Unit =
    chan.write(ByteBuffer.wrap(writeToArray(v)))
    ()

  protected class ReadHandler[T](onRead: T => Unit)(using JsonValueCodec[T])
      extends CompletionHandler[Integer, ByteBuffer]() {
    override def completed(result: Integer, attachment: ByteBuffer): Unit = {
      val buffer = attachment
      buffer.flip()
      onRead(readFromByteBuffer(buffer))
      buffer.clear()

      chan.read(buffer, buffer, this)
    }

    override def failed(exc: Throwable, attachment: ByteBuffer): Unit = {
      exc match {
        case _: ClosedChannelException =>
        case _ =>
          println("failed called with exception: ")
          exc.printStackTrace()
      }
    }
  }

  override def read[T: JsonValueCodec](onRead: T => Unit): Unit = {
    val recvBuffer = ByteBuffer.allocate(10000)
    chan.read(recvBuffer, recvBuffer, new ReadHandler(onRead))
  }
}
