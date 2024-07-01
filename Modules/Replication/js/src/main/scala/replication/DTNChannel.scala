package replication

import channels.{Abort, ArrayMessageBuffer, ConnectionContext, Incoming, LatentConnection, MessageBuffer}
import de.rmgk.delay
import de.rmgk.delay.syntax.toAsync
import de.rmgk.delay.{Async, Callback}
import dtn.{Bundle, BundleCreation, PayloadBlock, WSEndpointClient}
import replication.DTNChannel.cRDTGroupEndpoint

import scala.concurrent.ExecutionContext
import scala.util.{Failure, Success}

object DTNChannel {
  val cRDTGroupEndpoint: String = "dtn://global/~rdt/app1"
}

class DTNwsEndpointContext(connection: WSEndpointClient, executionContext: ExecutionContext) extends ConnectionContext {

  def makeBundle(messageBuffer: MessageBuffer): Bundle =
    BundleCreation.createBundle(
      data = messageBuffer.asArray,
      full_destination_uri = cRDTGroupEndpoint,
      full_source_uri = cRDTGroupEndpoint
    )

  override def send(message: MessageBuffer): Async[Any, Unit] =
    connection.sendBundle(makeBundle(message)).toAsync(using executionContext)

  override def close(): Unit = connection.disconnect().onComplete {
    case Failure(f)     => f.printStackTrace()
    case Success(value) => ()
  }(using executionContext)
}

class DTNChannel(host: String, port: Int, ec: ExecutionContext) extends LatentConnection {

  def receiveBundle(ws: WSEndpointClient, cb: Callback[MessageBuffer]): Unit = {
    ws.receiveBundle().onComplete(bundle => {
      for block <- bundle.get.other_blocks do {
        block match
          case PayloadBlock(block_type_code, block_number, block_processing_control_flags, crc_type, data) =>
            cb.succeed(ArrayMessageBuffer(block.data))
          case _ =>
      }
      receiveBundle(ws, cb)
    })(using ec)
  }

  override def prepare(incoming: Incoming): Async[Abort, ConnectionContext] = Async {
    val ws   = WSEndpointClient(host, port).toAsync(using ec).bind
    val conn = DTNwsEndpointContext(ws, ec)
    val cb   = incoming(conn)
    receiveBundle(ws, cb)
    ws.registerEndpointAndSubscribe(cRDTGroupEndpoint).toAsync(using ec).bind
    conn
  }
}
