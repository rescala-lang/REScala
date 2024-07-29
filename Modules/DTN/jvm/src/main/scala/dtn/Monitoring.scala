package dtn

import io.bullet.borer.Json

import java.io.BufferedOutputStream
import java.nio.charset.StandardCharsets
import java.nio.file.Files
import java.nio.file.Paths
import java.nio.file.StandardOpenOption
import java.time.ZoneId
import java.time.ZonedDateTime
import scala.util.Using

class MonitoringServer(server: TCPReadonlyServer) {
  def run(): Unit = {
    val dir = Paths.get("/shared/monitoring")

    Using.Manager { use =>
      val streamReceived =
        use(BufferedOutputStream(Files.newOutputStream(dir.resolve("received.data"), StandardOpenOption.APPEND)))
      val streamForwarded =
        use(BufferedOutputStream(Files.newOutputStream(dir.resolve("forwarded.data"), StandardOpenOption.APPEND)))
      val streamDelivered =
        use(BufferedOutputStream(Files.newOutputStream(dir.resolve("delivered.data"), StandardOpenOption.APPEND)))
      val streamCreated =
        use(BufferedOutputStream(Files.newOutputStream(dir.resolve("created.data"), StandardOpenOption.APPEND)))

      try {
        while true do {
          val (connection, data) = server.queue.take()

          val now: ZonedDateTime = ZonedDateTime.now(ZoneId.of("UTC"))

          Json.decode(data).to[MonitoringMessage].value match
            case m: MonitoringMessage.BundleReceivedAtRouter =>
              streamReceived.write(Json.encode[MonitoringMessage](m.copy(time = Option(now))).toByteArray)
              streamReceived.write("\n".getBytes())
              streamReceived.flush()
            case m: MonitoringMessage.BundleForwardedAtRouter =>
              streamForwarded.write(Json.encode[MonitoringMessage](m.copy(time = Option(now))).toByteArray)
              streamForwarded.write("\n".getBytes())
              streamForwarded.flush()
            case m: MonitoringMessage.BundleDeliveredAtClient =>
              streamDelivered.write(Json.encode[MonitoringMessage](m.copy(time = Option(now))).toByteArray)
              streamDelivered.write("\n".getBytes())
              streamDelivered.flush()
            case m: MonitoringMessage.BundleCreatedAtClient =>
              streamCreated.write(Json.encode[MonitoringMessage](m.copy(time = Option(now))).toByteArray)
              streamCreated.write("\n".getBytes())
              streamCreated.flush()
        }
      } catch {
        case e: Exception =>
          println(s"monitoring server ran into exception: $e")
          server.stop()
      }
    }
    ()
  }
}
object MonitoringServer {
  def apply(interface: String, port: Int): MonitoringServer = {
    val server = TCPReadonlyServer(interface, port)
    server.start()
    println(s"monitoring server started under $interface:$port")
    new MonitoringServer(server)
  }
}

class MonitoringClient(connection: TCPConnection) extends MonitoringClientInterface {
  override def send(message: MonitoringMessage): Unit = {
    connection.send(Json.encode[MonitoringMessage](message).toByteArray)
  }
}
object MonitoringClient {
  def apply(host: String, port: Int): MonitoringClient = {
    new MonitoringClient(TCPConnection(host, port))
  }
}
