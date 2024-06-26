package replication

import channels.{Abort, ArrayMessageBuffer, ConnectionContext, Incoming, LatentConnection, MessageBuffer}
import de.rmgk.delay
import de.rmgk.delay.syntax.toAsync
import de.rmgk.delay.{Async, Callback}
import rdts.time.Dots
import dtn.RdtClient

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
    // @rmgk: TODO creating the Rdt client starts receiving, thus there is a window in which messages will be lost
    val client: RdtClient = RdtClient.create(host, port, appName).toAsync(using ec).bind
    val conn = DTNRdtClientContext(client, ec)
    val cb = incoming(conn)

    client.registerOnReceive((payload: Array[Byte], dots: Dots) => cb.succeed(ArrayMessageBuffer(payload)))
    conn
  }
}
