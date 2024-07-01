package dtn

import rdts.time.Dots
import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global


class RdtClient(ws: WSEndpointClient, cc: DotsConvergenceClient, appName: String) {
  var _callback: Option[(Array[Byte], Dots) => Unit] = None

  def send(payload: Array[Byte], dots: Dots): Future[Unit] = {
    val bundle: Bundle = BundleCreation.createBundleRdt(
      data = payload,
      dots = dots,
      node = Endpoint.createFrom(ws.nodeId),
      full_destination_uri = s"dtn://global/~rdt/${appName}",
      full_source_uri = s"${ws.nodeId}rdt/${appName}"
    )

    ws.sendBundle(bundle)
  }

  def registerOnReceive(callback: (Array[Byte], Dots) => Unit) = {
    _callback = Option(callback)
  }
}
object RdtClient {
  def apply(host: String, port: Int, appName: String, checkerHost: String, checkerPort: Int): Future[RdtClient] = {
    val checkerClient = DotsConvergenceClient(checkerHost, checkerPort)

    WSEndpointClient(host, port)
      .flatMap(ws => ws.registerEndpointAndSubscribe(s"dtn://global/~rdt/${appName}"))
      .flatMap(ws => ws.registerEndpointAndSubscribe(s"${ws.nodeId}rdt/${appName}"))
      .map(ws => {
        val client = new RdtClient(ws, checkerClient, appName)

        // flush receive forever and call callback if available
        def flush_receive(): Future[Bundle] = {
          ws.receiveBundle().flatMap(bundle => {
            println(s"received bundle: ${bundle.id}")

            val payload: Option[Array[Byte]] = bundle.other_blocks.collectFirst({ case x: PayloadBlock => x.data })
            val dots: Option[Dots] = bundle.other_blocks.collectFirst({ case x: RdtMetaBlock => x.dots })

            if (payload.isEmpty || dots. isEmpty) {
              println("did not contain dots or payload. bundle is no rdt bundle. ignoring bundle.")
            } else {
              checkerClient.send(dots.get)

              client._callback match
                case None => println("no callback set. could not deliver rdt data. ignoring bundle.")
                case Some(callback) => callback(payload.get, dots.get)
            }

            flush_receive()
          })
        }
        flush_receive().recover(throwable => println(throwable))

        client
      })
  }
}


