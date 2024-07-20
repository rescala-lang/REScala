package replication

import _root_.dtn.{NoDotsConvergenceClient, RdtClient}
import channels.{Abort, Connection, LatentConnection}
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
        // TODO: how to handle this? Optimally, this should be answered by sending all known payloads that are larger than the provided set of dots
        Sync { () }
      case ProtocolMessage.Payload(sender, dots, data) =>
        connection.send(writeToArray[T](data), dots).toAsync(using executionContext)

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

  override def prepare(incomingHandler: Handler)
      : Async[Abort, Connection[ProtocolMessage[T]]] = Async {
    val client: RdtClient = RdtClient(host, port, appName, NoDotsConvergenceClient).toAsync(using ec).bind
    val conn              = DTNRdtClientContext[T](client, ec)
    val cb                = incomingHandler(conn)

    client.registerOnReceive { (payload: Array[Byte], dots: Dots) =>
      val data = readFromArray[T](payload)
      cb.succeed(ProtocolMessage.Payload(dtnid, dots, data))
    }

    // TODO: create custom empty request to signal to the application that it should send us all data it has.
    // optimally, this should not be an empty set of dots, but those present in the network
    cb.succeed(ProtocolMessage.Request(dtnid, Dots.empty))

    conn
  }
}
