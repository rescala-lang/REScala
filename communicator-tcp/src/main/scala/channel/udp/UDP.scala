package channel.udp

import channel.{ArrayMessageBuffer, Bidirectional, InChan, MessageBuffer, OutChan}
import de.rmgk.delay.Async

import java.io.{BufferedInputStream, BufferedOutputStream, IOException}
import java.net.{DatagramPacket, DatagramSocket, InetAddress, InetSocketAddress, ServerSocket, Socket, SocketAddress, SocketException}
import java.util.concurrent.{Executors, ScheduledFuture, ThreadFactory, TimeUnit}
import scala.collection.mutable
import scala.util.control.NonFatal
import scala.util.{Failure, Success}


class Ctx(@volatile var closeRequest: Boolean = false)

type Prod[A] = Async[Ctx, A]

class UDPOutChan(address: SocketAddress) extends OutChan {

  val clientSocket: DatagramSocket = new DatagramSocket()

  override def send(message: MessageBuffer): Async[Any, Unit] = Async {
    // Create a packet with the message, server address, and port
    val sendPacket: DatagramPacket = new DatagramPacket(message.asArray, message.length, address)

    // Send the packet to the server
    clientSocket.send(sendPacket)
  }
  override def close(): Unit = clientSocket.close()
}

class UdpInChan(port: Int) extends InChan {

  val serverSocket = new DatagramSocket(port)

  override def receive: Prod[MessageBuffer] = Async.fromCallback {

    val receiveBuffer = new Array[Byte](1 << 20)

    try
      while true do
        val packet = new DatagramPacket(receiveBuffer, receiveBuffer.length)
        serverSocket.receive(packet)
        Async.handler.succeed(ArrayMessageBuffer(packet.getData.slice(packet.getOffset, packet.getLength)))
    catch
      case NonFatal(e) =>
        serverSocket.close()
        Async.handler.fail(e)
  }
  override def close(): Unit = serverSocket.close()
}
