package replication

import _root_.dtn.{NoDotsConvergenceClient, RdtClient, RdtMessageType}
import channels.{Abort, Connection, Handler, LatentConnection, MessageBuffer}
import com.github.plokhotnyuk.jsoniter_scala.core.{JsonValueCodec, readFromArray, writeToArray}
import de.rmgk.delay
import de.rmgk.delay.syntax.toAsync
import de.rmgk.delay.{Async, Callback, Sync}
import rdts.base.Uid
import rdts.time.Dots

import scala.concurrent.ExecutionContext
import scala.util.{Failure, Success}

class DTNRdtClientContext[T: JsonValueCodec](connection: RdtClient, executionContext: ExecutionContext)
    extends Connection[ProtocolMessage[T]] {
  override def send(message: ProtocolMessage[T]): Async[Any, Unit] =
    message match
      case ProtocolMessage.Request(sender, dots) =>
        connection.send(RdtMessageType.Request, Array(), dots).toAsync(using executionContext)
      case ProtocolMessage.Payload(sender, dots, data) =>
        connection.send(RdtMessageType.Payload, writeToArray[T](data), dots).toAsync(using executionContext)

  override def close(): Unit = connection.close().onComplete {
    case Failure(f)     => f.printStackTrace()
    case Success(value) => ()
  }(using executionContext)
}

class DTNChannel[T: JsonValueCodec](host: String, port: Int, appName: String, ec: ExecutionContext)
    extends LatentConnection[ProtocolMessage[T]] {

  // We use a local dtnid instead of a remote replica ID to signify that the local DTNd is the one providing information.
  // If the local dtnd could be stopped and restarted without loosing data, this id should remain the same for performance reasons, but it will be correct even if it changes.
  val dtnid = Uid.gen()

  override def prepare(incomingHandler: Handler[ProtocolMessage[T]]): Async[Abort, Connection[ProtocolMessage[T]]] =
    Async {
      val client: RdtClient = RdtClient(host, port, appName, NoDotsConvergenceClient).toAsync(using ec).bind
      val conn              = DTNRdtClientContext[T](client, ec)
      val cb                = incomingHandler.getCallbackFor(conn)

      client.registerOnReceive { (message_type: RdtMessageType, payload: Array[Byte], dots: Dots) =>
        message_type match
          case RdtMessageType.Request => cb.succeed(ProtocolMessage.Request(dtnid, dots))
          case RdtMessageType.Payload => cb.succeed(ProtocolMessage.Payload(dtnid, dots, readFromArray[T](payload)))
      }

      // TODO: create custom empty request to signal to the application that it should send us all data it has.
      // optimally, this should not be an empty set of dots, but those present in the network
      cb.succeed(ProtocolMessage.Request(dtnid, Dots.empty))

      conn
    }
}
