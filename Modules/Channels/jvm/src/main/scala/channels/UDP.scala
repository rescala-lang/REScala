package channels

import channels.*
import de.rmgk.delay.{Async, Callback, Sync}

import java.net.{DatagramPacket, DatagramSocket, SocketAddress, SocketTimeoutException}
import scala.concurrent.ExecutionContext
import scala.util.Success
import scala.util.control.NonFatal

object UDP {
  def sendreceive(target: SocketAddress, port: Int, executionContext: ExecutionContext) =
    new UDPPseudoConnection(target, () => new DatagramSocket(port), executionContext)

}

class UDPPseudoConnection(
    address: SocketAddress,
    socketFactory: () => DatagramSocket,
    executionContext: ExecutionContext
) extends LatentConnection {
  override def prepare(incoming: Incoming): Async[Abort, ConnectionContext] = Sync {

    val datagramWrapper = UDPDatagramWrapper(address, socketFactory())

    val messageCallback = incoming(datagramWrapper)
    executionContext.execute(() => datagramWrapper.receiveLoop(context, messageCallback))

    datagramWrapper

  }

}

class UDPDatagramWrapper(address: SocketAddress, datagramSocket: DatagramSocket) extends ConnectionContext {
  def send(message: MessageBuffer): Async[Any, Unit] = Async {
    // Create a packet with the message, server address, and port
    val sendPacket: DatagramPacket =
      val outArray = message.asArray
      new DatagramPacket(outArray, outArray.length, address)

    // Send the packet to the server
    datagramSocket.send(sendPacket)
  }

  override def close(): Unit = datagramSocket.close()

  def receiveLoop(abort: Abort, messageCallback: Callback[MessageBuffer]) =
    // 1 << 16 should be slightly larger than the max UDP/IP packets
    val receiveBuffer = new Array[Byte](1 << 16)

    try { // scalafmt does not understand this without braces
      try {
        while !abort.closeRequest do
          val packet = new DatagramPacket(receiveBuffer, receiveBuffer.length)
          try
            datagramSocket.receive(packet)
            messageCallback.succeed(ArrayMessageBuffer(packet.getData.slice(packet.getOffset, packet.getLength)))
          catch case _: SocketTimeoutException => ()
      } finally { datagramSocket.close() }
    } catch
      case NonFatal(e) =>
        messageCallback.fail(e)
}
