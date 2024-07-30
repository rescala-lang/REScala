package dtn

import io.bullet.borer.Codec
import io.bullet.borer.derivation.MapBasedCodecs.*
import rdts.time.Dots
import java.time.ZonedDateTime
import io.bullet.borer.{Encoder, Decoder}

enum MonitoringMessage derives Codec.All:
  case BundleReceivedAtRouter(nodeId: String, bundleId: String, time: Option[ZonedDateTime] = None)
  case BundleForwardedAtRouter(nodeId: String, bundleId: String, time: Option[ZonedDateTime] = None)
  case BundleDeliveredAtClient(clientId: String, bundleId: String, dots: Dots, time: Option[ZonedDateTime] = None)
  case BundleCreatedAtClient(clientId: String, bundleId: String, dots: Dots, time: Option[ZonedDateTime] = None)

trait MonitoringClientInterface {
  def send(message: MonitoringMessage): Unit
}

object NoMonitoringClient extends MonitoringClientInterface {
  override def send(message: MonitoringMessage): Unit = ()
}

// default formatter of ZonedDateTime should suffice here
given Encoder[ZonedDateTime] = Encoder { (writer, zonedDateTime) =>
  writer.write(zonedDateTime.toString())
}
given Decoder[ZonedDateTime] = Decoder { reader =>
  ZonedDateTime.parse(reader.readString())
}
