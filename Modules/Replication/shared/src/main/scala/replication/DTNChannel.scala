package replication

import _root_.dtn.{NoDotsConvergenceClient, RdtClient}
import channels.{Abort, AbstractConnectionContext, AbstractIncoming, AbstractLatentConnection}
import com.github.plokhotnyuk.jsoniter_scala.core.{JsonValueCodec, readFromArray, writeToArray}
import de.rmgk.delay
import de.rmgk.delay.syntax.toAsync
import de.rmgk.delay.{Async, Callback, Sync}
import rdts.base.Uid
import rdts.time.Dots

import scala.concurrent.ExecutionContext
import scala.util.{Failure, Success}

class DTNRdtClientContext[T: JsonValueCodec](connection: RdtClient, executionContext: ExecutionContext)
    extends AbstractConnectionContext[ProtocolMessage[T]] {
  override def send(message: ProtocolMessage[T]): Async[Any, Unit] =
    message match
      case ProtocolMessage.Request(sender, dots) =>
        // TODO: how to handle this? We
        Sync { () }
      case ProtocolMessage.Payload(sender, dots, data) =>
        connection.send(writeToArray[T](data), dots).toAsync(using executionContext)

  override def close(): Unit = connection.close().onComplete {
    case Failure(f)     => f.printStackTrace()
    case Success(value) => ()
  }(using executionContext)
}

class DTNChannel[T: JsonValueCodec](host: String, port: Int, appName: String, ec: ExecutionContext)
    extends AbstractLatentConnection[ProtocolMessage[T]] {
  override def prepare(incoming: AbstractIncoming[ProtocolMessage[T]])
      : Async[Abort, AbstractConnectionContext[ProtocolMessage[T]]] = Async {
    val client: RdtClient = RdtClient(host, port, appName, NoDotsConvergenceClient).toAsync(using ec).bind
    val conn              = DTNRdtClientContext[T](client, ec)
    val cb                = incoming(conn)

    client.registerOnReceive { (payload: Array[Byte], dots: Dots) =>
      val data = readFromArray[T](payload)
      // TODO: is Uid zero bad?
      cb.succeed(ProtocolMessage.Payload(Uid.zero, dots, data))
    }

    // TODO: create custom empty request to signal to the application that it should send us all data it has.
    // optimally, this should not be an empty set of dots, but those present in the network
    cb.succeed(ProtocolMessage.Request(Uid.zero, Dots.empty))

    conn
  }
}
