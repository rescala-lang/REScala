package channel.udp

import channel.{Abort, ArrayMessageBuffer, ConnectionContext, Incoming, LatentConnection, MessageBuffer, Prod, context}
import de.rmgk.delay.Async

import java.net.{DatagramPacket, DatagramSocket, InetSocketAddress, SocketAddress, SocketTimeoutException}
import scala.concurrent.ExecutionContext
import scala.concurrent.duration.Duration
import scala.util.Success
import scala.util.control.NonFatal

object UDP {
  def sendreceive(target: SocketAddress, port: Int, executionContext: ExecutionContext) =
    new UDPPseudoConnection(target, new DatagramSocket(port), executionContext)

}

class UDPPseudoConnection(address: SocketAddress, val datagramSocket: DatagramSocket, executionContext: ExecutionContext)
    extends ConnectionContext with LatentConnection {
  override def prepare(incoming: Incoming): Async[Abort, ConnectionContext] = Async.fromCallback {

    executionContext.execute { () =>
      val messageCallback = incoming(this)

      // 1 << 16 should be slightly larger than the max UDP/IP packets
      val receiveBuffer = new Array[Byte](1 << 16)

      try { // scalafmt does not understand this without braces
        while !context.closeRequest do
          val packet = new DatagramPacket(receiveBuffer, receiveBuffer.length)
          try
            datagramSocket.receive(packet)
            messageCallback.succeed(ArrayMessageBuffer(packet.getData.slice(packet.getOffset, packet.getLength)))
          catch case _: SocketTimeoutException => ()
          finally
            datagramSocket.close()
      } catch
        case NonFatal(e) =>
          Async.handler.fail(e)
    }

  }

  def send(message: MessageBuffer): Async[Any, Unit] = Async {
    // Create a packet with the message, server address, and port
    val sendPacket: DatagramPacket =
      val outArray = message.asArray
      new DatagramPacket(outArray, outArray.length, address)

    // Send the packet to the server
    datagramSocket.send(sendPacket)
  }

  override def close(): Unit = datagramSocket.close()
}
