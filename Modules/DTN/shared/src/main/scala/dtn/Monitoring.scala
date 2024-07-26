package dtn

import io.bullet.borer.Codec
import io.bullet.borer.derivation.MapBasedCodecs.*
import rdts.time.Dots

enum MonitoringMessage derives Codec.All:
  case BundleReceivedAtRouter(nodeId: String, bundleId: String)
  case BundleForwardedAtRouter(nodeId: String, bundleId: String)
  case BundleDeliveredAtClient(clientId: String, bundleId: String, dots: Dots)
  case BundleCreatedAtClient(clientId: String, bundleId: String, dots: Dots)

trait MonitoringClientInterface {
  def send(message: MonitoringMessage): Unit
}

object NoMonitoringClient extends MonitoringClientInterface {
  override def send(message: MonitoringMessage): Unit = ()
}
