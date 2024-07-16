package dtn

import rdts.time.Dots

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

class RdtClient(ws: WSEndpointClient, appName: String, checkerClient: ConvergenceClientInterface) {
  def send(payload: Array[Byte], dots: Dots): Future[Unit] = {
    val bundle: Bundle = BundleCreation.createBundleRdt(
      data = payload,
      dots = dots,
      node = Endpoint.createFrom(ws.nodeId),
      full_destination_uri = s"dtn://global/~rdt/$appName",
      full_source_uri = s"${ws.nodeId}rdt/$appName"
    )

    ws.sendBundle(bundle)
  }

  def registerOnReceive(callback: (Array[Byte], Dots) => Unit): Unit = {
    // flush receive forever and call callback
    def flush_receive(): Future[Bundle] = {
      ws.receiveBundle().flatMap(bundle => {
        println(s"received bundle: ${bundle.id}")

        val payload: Option[Array[Byte]] = bundle.other_blocks.collectFirst({ case x: PayloadBlock => x.data })
        val dots: Option[Dots]           = bundle.other_blocks.collectFirst({ case x: RdtMetaBlock => x.dots })

        if payload.isEmpty || dots.isEmpty then {
          println("did not contain dots or payload. bundle is no rdt bundle. ignoring bundle.")
        } else {
          checkerClient.send(dots.get)
          callback(payload.get, dots.get)
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
