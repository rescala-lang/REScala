package dtn

import io.bullet.borer.Json

import java.io.BufferedOutputStream
import java.nio.charset.StandardCharsets
import java.nio.file.Files
import java.nio.file.Paths
import java.nio.file.StandardOpenOption
import java.time.ZoneId
import java.time.ZonedDateTime
import java.time.Duration
import scala.util.Using
import java.io.BufferedInputStream
import java.util.Scanner
import java.io.File
import rdts.time.Dots
import rdts.base.Lattice.syntax

val MONITORING_DIR                = Paths.get("/shared/monitoring")
val RECEIVED_DATA_FP              = MONITORING_DIR.resolve("received.data")
val FORWARDED_DATA_FP             = MONITORING_DIR.resolve("forwarded.data")
val CREATED_AND_DELIVERED_DATA_FP = MONITORING_DIR.resolve("created_and_delivered.data")

class MonitoringServer(server: TCPReadonlyServer) {
  def run(): Unit = {
    val dir = Paths.get("/shared/monitoring")

    Using.Manager { use =>
      val streamReceived =
        use(BufferedOutputStream(Files.newOutputStream(RECEIVED_DATA_FP, StandardOpenOption.APPEND)))
      val streamForwarded =
        use(BufferedOutputStream(Files.newOutputStream(FORWARDED_DATA_FP, StandardOpenOption.APPEND)))
      val streamCreatedAndDelivered =
        use(BufferedOutputStream(Files.newOutputStream(CREATED_AND_DELIVERED_DATA_FP, StandardOpenOption.APPEND)))

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
              streamCreatedAndDelivered.write(Json.encode[MonitoringMessage](m.copy(time = Option(now))).toByteArray)
              streamCreatedAndDelivered.write("\n".getBytes())
              streamCreatedAndDelivered.flush()
            case m: MonitoringMessage.BundleCreatedAtClient =>
              streamCreatedAndDelivered.write(Json.encode[MonitoringMessage](m.copy(time = Option(now))).toByteArray)
              streamCreatedAndDelivered.write("\n".getBytes())
              streamCreatedAndDelivered.flush()
        }
      } catch {
        case e: Exception =>
          println(s"monitoring server ran into exception: $e")
      } finally {
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

class MonitoringBundlesReceivedPrinter {
  def run(): Unit = {
    Using(Scanner(BufferedInputStream(Files.newInputStream(RECEIVED_DATA_FP, StandardOpenOption.READ)))) { in =>
      in.useDelimiter("\n")

      var oldTime: Option[ZonedDateTime] = None

      var messageCount: Long = 0

      while in.hasNext() do {
        Json.decode(in.next().getBytes()).to[MonitoringMessage].value match
          case MonitoringMessage.BundleForwardedAtRouter(nodeId, bundleId, time)         => ()
          case MonitoringMessage.BundleDeliveredAtClient(clientId, bundleId, dots, time) => ()
          case MonitoringMessage.BundleCreatedAtClient(clientId, bundleId, dots, time)   => ()
          case MonitoringMessage.BundleReceivedAtRouter(nodeId, bundleId, time) => {
            if oldTime.isEmpty then oldTime = time

            if Duration.between(time.get, oldTime.get).toSeconds() >= 1 then {
              print("\u001b[2J") // clear console screen
              println("Total Messages Received\n")
              println(s"${time.get}: ${messageCount}")
              oldTime = time
              messageCount = 1
            } else {
              messageCount += 1
            }
          }
      }
    }

    ()
  }
}

class MonitoringBundlesForwardedPrinter {
  def run(): Unit = {
    Using(Scanner(BufferedInputStream(Files.newInputStream(FORWARDED_DATA_FP, StandardOpenOption.READ)))) { in =>
      in.useDelimiter("\n")

      var oldTime: Option[ZonedDateTime] = None

      var messageCount: Long = 0

      while in.hasNext() do {
        Json.decode(in.next().getBytes()).to[MonitoringMessage].value match
          case MonitoringMessage.BundleDeliveredAtClient(clientId, bundleId, dots, time) => ()
          case MonitoringMessage.BundleCreatedAtClient(clientId, bundleId, dots, time)   => ()
          case MonitoringMessage.BundleReceivedAtRouter(nodeId, bundleId, time)          => ()
          case MonitoringMessage.BundleForwardedAtRouter(nodeId, bundleId, time) => {
            if oldTime.isEmpty then oldTime = time

            if Duration.between(time.get, oldTime.get).toSeconds() >= 1 then {
              print("\u001b[2J") // clear console screen
              println("Total Messages Forwarded\n")
              println(s"${time.get}: ${messageCount}")
              oldTime = time
              messageCount = 1
            } else {
              messageCount += 1
            }
          }
      }
    }

    ()
  }
}

class MonitoringStateDevelopmentPrinter {
  def run(creationClientId: String): Unit = {
    Using(Scanner(BufferedInputStream(Files.newInputStream(CREATED_AND_DELIVERED_DATA_FP, StandardOpenOption.READ)))) {
      in =>
        in.useDelimiter("\n")

        var creationState: Dots                = Dots.empty
        var deliveredStates: Map[String, Dots] = Map()

        // these count anomalies that should not happen!?
        var bundlesCreatedAtOtherNodesCounter: Long = 0
        var bundlesDeliveredAtCreationCounter: Long = 0

        var newestTime: Option[ZonedDateTime] = None

        while in.hasNext() do {
          Json.decode(in.next().getBytes()).to[MonitoringMessage].value match
            case MonitoringMessage.BundleDeliveredAtClient(clientId, bundleId, dots, time) => {
              if clientId == creationClientId then {
                bundlesDeliveredAtCreationCounter += 1
              } else {
                deliveredStates = deliveredStates.merge(Map(clientId -> dots))
              }
              newestTime = time
            }
            case MonitoringMessage.BundleCreatedAtClient(clientId, bundleId, dots, time) => {
              if clientId != creationClientId then {
                bundlesCreatedAtOtherNodesCounter += 1
              } else {
                creationState = creationState.merge(dots)
              }
              newestTime = time
            }
            case MonitoringMessage.BundleReceivedAtRouter(nodeId, bundleId, time)  => ()
            case MonitoringMessage.BundleForwardedAtRouter(nodeId, bundleId, time) => ()

          print("\u001b[2J") // clear console screen
          println(s"${newestTime}\n")
          println("States Ratio of num-dots")
          for (clientId: String, dots: Dots) <- deliveredStates do {
            val ratio: Double = dots.toSet.size.toDouble / creationState.toSet.size.toDouble
            println(s"${clientId}: ${ratio}")
          }
          println(s"\nNum bundles created at other nodes: ${bundlesCreatedAtOtherNodesCounter}")
          println(s"Num bundles delivered at creation node: ${bundlesDeliveredAtCreationCounter}")
        }
    }

    ()
  }
}
