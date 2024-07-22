package dtn

import rdts.time.Dots

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

class RdtClient(ws: WSEndpointClient, appName: String, checkerClient: ConvergenceClientInterface) {
  def send(message_type: RdtMessageType, payload: Array[Byte], dots: Dots): Future[Unit] = {
    val bundle: Bundle = BundleCreation.createBundleRdt(
      message_type = message_type,
      data = payload,
      dots = dots,
      node = Endpoint.createFrom(ws.nodeId),
      full_destination_uri = s"dtn://global/~rdt/$appName",
      full_source_uri = s"${ws.nodeId}rdt/$appName"
    )

    ws.sendBundle(bundle)
  }

  def registerOnReceive(callback: (RdtMessageType, Array[Byte], Dots) => Unit): Unit = {
    // flush receive forever and call callback
    def flush_receive(): Future[Bundle] = {
      ws.receiveBundle().flatMap(bundle => {
        println(s"received bundle: ${bundle.id}")

        val payload: Option[Array[Byte]]       = bundle.other_blocks.collectFirst({ case x: PayloadBlock => x.data })
        val rdt_meta_info: Option[RdtMetaInfo] = bundle.other_blocks.collectFirst({ case x: RdtMetaBlock => x.info })

        if payload.isEmpty || rdt_meta_info.isEmpty then {
          println("did not contain dots or payload. bundle is no rdt bundle. ignoring bundle.")
        } else {
          checkerClient.send(rdt_meta_info.get.dots)
          callback(rdt_meta_info.get.message_type, payload.get, rdt_meta_info.get.dots)
        }

        flush_receive()
      })
    }
    flush_receive().recover(throwable => println(throwable))
    ()
  }

  def close(): Future[Unit] = ws.disconnect()
}
object RdtClient {
  def apply(
      host: String,
      port: Int,
      appName: String,
      checkerClient: ConvergenceClientInterface = NoDotsConvergenceClient
  ): Future[RdtClient] = {
    WSEndpointClient(host, port)
      .flatMap(ws => ws.registerEndpointAndSubscribe(s"dtn://global/~rdt/$appName"))
      .flatMap(ws => ws.registerEndpointAndSubscribe(s"${ws.nodeId}rdt/$appName"))
      .map(ws => new RdtClient(ws, appName, checkerClient))
  }
}
