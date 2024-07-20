package channels

import channels.*
import de.rmgk.delay.{Async, Callback, Sync}

import java.net.{DatagramPacket, DatagramSocket, InetSocketAddress, SocketAddress, SocketTimeoutException}
import scala.collection.mutable
import scala.concurrent.ExecutionContext
import scala.util.control.NonFatal
import scala.util.{Failure, Success}

object UDP {
  def listen(socketFactory: () => DatagramSocket, executionContext: ExecutionContext): UDPPseudoConnection =
    new UDPPseudoConnection(socketFactory, executionContext, Async.fromCallback(()))

  def connect(
      target: SocketAddress,
      socketFactory: () => DatagramSocket,
      executionContext: ExecutionContext
  ): UDPPseudoConnection =
    new UDPPseudoConnection(socketFactory, executionContext, Sync(target))
}

class UDPPseudoConnection(
    socketFactory: () => DatagramSocket,
    executionContext: ExecutionContext,
    initializeOutbound: Async[Any, SocketAddress],
) extends LatentConnection[MessageBuffer] {
  override def prepare(incomingHandler: Handler[MessageBuffer]): Async[Abort, Connection[MessageBuffer]] =
    Async.fromCallback[Connection[MessageBuffer]] {

      val datagramSocket = socketFactory()
      val receiveBuffer  = new Array[Byte](1 << 16)

      val connectionSuccess: Callback[Connection[MessageBuffer]] = Async.handler[Connection[MessageBuffer]]

      val connections: mutable.Map[SocketAddress, (UDPDatagramWrapper, Callback[MessageBuffer])] = mutable.Map.empty

      def getOrCreateConnection(sa: SocketAddress) = connections.synchronized {
        connections.getOrElseUpdate(
          sa, {
            val dw = UDPDatagramWrapper(sa, datagramSocket)
            connectionSuccess.succeed(dw)
            val cb = incomingHandler.getCallbackFor(dw)
            (dw, cb)
          }
        )
      }

      def receiveLoop(abort: Abort): Unit = {
        // 1 << 16 should be slightly larger than the max UDP/IP packets
        try { // scalafmt does not understand this without braces
          try {
            while !abort.closeRequest do
              val packet = new DatagramPacket(receiveBuffer, receiveBuffer.length)
              try
                datagramSocket.receive(packet)

                val sa = packet.getSocketAddress

                val (conn, receiveCallback) = getOrCreateConnection(sa)

                receiveCallback.succeed(ArrayMessageBuffer(packet.getData.slice(packet.getOffset, packet.getLength)))
              catch case _: SocketTimeoutException => ()
          } finally { datagramSocket.close() }
        } catch
          case NonFatal(e) =>
            connections.values.foreach: (_, cb) =>
              cb.fail(e)
      }

      executionContext.execute(() => receiveLoop(summon[Abort]))

      initializeOutbound.run:
        case Success(target) =>
          getOrCreateConnection(target)
          ()
        case Failure(exception) => connectionSuccess.fail(exception)

    }

}

class UDPDatagramWrapper(target: SocketAddress, datagramSocket: DatagramSocket)
    extends Connection[MessageBuffer] {

  override val info: ConnectionInfo =
    datagramSocket.getLocalSocketAddress match
      case isa: InetSocketAddress =>
        ConnectionInfo(Option(isa.getHostString), Option(isa.getPort))
      case other => ConnectionInfo(None, None)

  def send(message: MessageBuffer): Async[Any, Unit] = Async {
    // Create a packet with the message, server address, and port
    val sendPacket: DatagramPacket =
      val outArray = message.asArray
      new DatagramPacket(outArray, outArray.length, target)

    // Send the packet to the server
    datagramSocket.send(sendPacket)
  }

  override def close(): Unit = datagramSocket.close()

}
