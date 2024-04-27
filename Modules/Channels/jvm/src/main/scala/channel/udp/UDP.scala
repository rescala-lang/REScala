package channel.udp

import channel.{ArrayMessageBuffer, InChan, MessageBuffer, OutChan, Prod, context}
import de.rmgk.delay.Async

import java.net.{DatagramPacket, DatagramSocket, SocketAddress, SocketTimeoutException}
import scala.concurrent.duration.Duration
import scala.util.control.NonFatal
import scala.util.{Failure, Success}

class UDPOutChan(address: SocketAddress, val clientSocket: DatagramSocket) extends OutChan {

  override def send(message: MessageBuffer): Async[Any, Unit] = Async {
    // Create a packet with the message, server address, and port
    val sendPacket: DatagramPacket =
      val outArray = message.asArray
      new DatagramPacket(outArray, outArray.length, address)

    // Send the packet to the server
    clientSocket.send(sendPacket)
  }
}

object UDPOutChan {
  def establish(address: SocketAddress) =
    new UDPOutChan(address, new DatagramSocket())
}

class UdpInChan(serverSocket: DatagramSocket) extends InChan {

  override def receive: Prod[MessageBuffer] = Async.fromCallback {

    // 1 << 16 should be slightly larger than the max UDP/IP packets
    val receiveBuffer = new Array[Byte](1 << 16)

    try { // scalafmt does not understand this without braces
      try
        while !context.closeRequest do
          val packet = new DatagramPacket(receiveBuffer, receiveBuffer.length)
          try
            serverSocket.receive(packet)
            Async.handler.succeed(ArrayMessageBuffer(packet.getData.slice(packet.getOffset, packet.getLength)))
          catch case _: SocketTimeoutException => ()
      finally
        serverSocket.close()
    } catch
      case NonFatal(e) =>
        Async.handler.fail(e)
  }
}

object UdpInChan {
  def listen(port: Int, timeout: Duration) = {
    val serverSocket = new DatagramSocket(port)
    if timeout.isFinite then serverSocket.setSoTimeout(timeout.toMillis.toInt)
    new UdpInChan(serverSocket)
  }
}
