package replication

import channels.{Abort, ArrayMessageBuffer, ConnectionContext, Incoming, LatentConnection, MessageBuffer}
import de.rmgk.delay
import de.rmgk.delay.syntax.toAsync
import de.rmgk.delay.{Async, Callback}
import rdts.time.Dots
import _root_.dtn.{RdtClient, NoDotsConvergenceClient}

import scala.concurrent.ExecutionContext
import scala.util.{Failure, Success}


class DTNRdtClientContext(connection: RdtClient, executionContext: ExecutionContext) extends ConnectionContext {
  override def send(message: MessageBuffer): Async[Any, Unit] =
    connection.send(message.asArray, Dots.empty).toAsync(using executionContext)

  override def close(): Unit = connection.close().onComplete {
    case Failure(f)     => f.printStackTrace()
    case Success(value) => ()
  }(using executionContext)
}

class DTNChannel(host: String, port: Int, appName: String, ec: ExecutionContext) extends LatentConnection {
  override def prepare(incoming: Incoming): Async[Abort, ConnectionContext] = Async {
    val client: RdtClient = RdtClient(host, port, appName, NoDotsConvergenceClient).toAsync(using ec).bind
    val conn = DTNRdtClientContext(client, ec)
    val cb = incoming(conn)

    client.registerOnReceive((payload: Array[Byte], dots: Dots) => cb.succeed(ArrayMessageBuffer(payload)))
    conn
  }
}
