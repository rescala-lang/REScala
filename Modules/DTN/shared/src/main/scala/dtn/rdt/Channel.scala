package dtn.rdt

import channels.{Abort, Connection, Receive, LatentConnection}
import com.github.plokhotnyuk.jsoniter_scala.core.{JsonValueCodec, readFromArray, writeToArray}
import de.rmgk.delay
import de.rmgk.delay.syntax.toAsync
import de.rmgk.delay.{Async, Callback, Sync}
import dtn.{MonitoringClientInterface, NoMonitoringClient, RdtMessageType}
import rdts.base.Uid
import rdts.time.Dots
import replication.ProtocolMessage
import replication.ProtocolMessage.{Payload, Ping, Pong, Request}

import scala.concurrent.ExecutionContext
import scala.util.{Failure, Success}

enum ClientOperationMode {
  case PushAll
  case RequestLater
}

class ClientContext[T: JsonValueCodec](
    connection: Client,
    executionContext: ExecutionContext,
    operationMode: ClientOperationMode
) extends Connection[ProtocolMessage[T]] {
  override def send(message: ProtocolMessage[T]): Async[Any, Unit] =
    message match
      case Request(sender, dots) =>
        // we could send requests into the network. the routing handles them correctly. but they are unnecessary with the cb.succeed() down below.
        // todo: actually there should be no requests being sent anymore then. is that the case?
        operationMode match
          case ClientOperationMode.PushAll => Sync { () }
          case ClientOperationMode.RequestLater =>
            connection.send(RdtMessageType.Request, Array(), dots).toAsync(using executionContext)
        Sync { () }
      case Payload(sender, dots, data) =>
        connection.send(RdtMessageType.Payload, writeToArray[T](data), dots).toAsync(using executionContext)
      case Ping(_) | Pong(_) => Async {}

  override def close(): Unit = connection.close().onComplete {
    case Failure(f)     => f.printStackTrace()
    case Success(value) => ()
  }(using executionContext)
}

class Channel[T: JsonValueCodec](
    host: String,
    port: Int,
    appName: String,
    ec: ExecutionContext,
    monitoringClient: MonitoringClientInterface = NoMonitoringClient,
    operationMode: ClientOperationMode = ClientOperationMode.PushAll
) extends LatentConnection[ProtocolMessage[T]] {

  // We use a local dtnid instead of a remote replica ID to signify that the local DTNd is the one providing information.
  // If the local dtnd could be stopped and restarted without loosing data, this id should remain the same for performance reasons, but it will be correct even if it changes.
  val dtnid = Uid.gen()

  override def prepare(receiver: Receive[ProtocolMessage[T]]): Async[Abort, Connection[ProtocolMessage[T]]] =
    Async {
      val client: Client = Client(host, port, appName, monitoringClient).toAsync(using ec).bind
      val conn           = ClientContext[T](client, ec, operationMode)
      val cb             = receiver.messageHandler(conn)

      client.registerOnReceive { (message_type: RdtMessageType, payload: Array[Byte], dots: Dots) =>
        message_type match
          case RdtMessageType.Request => cb.succeed(ProtocolMessage.Request(dtnid, dots))
          case RdtMessageType.Payload => cb.succeed(ProtocolMessage.Payload(dtnid, dots, readFromArray[T](payload)))
      }

      // This tells the rdt to send everything it has and new following stuff into the network.
      // It makes any requests unnecessary.
      operationMode match
        case ClientOperationMode.PushAll      => cb.succeed(ProtocolMessage.Request(dtnid, Dots.empty))
        case ClientOperationMode.RequestLater => {}

      conn
    }
}
