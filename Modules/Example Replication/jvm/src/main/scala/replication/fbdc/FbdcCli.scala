package replication.fbdc

import channel.BiChan
import channel.tcp.{TCP, TCPConnection}

import java.nio.file.Path
import java.util.concurrent.Executors
import scala.concurrent.{ExecutionContext, Future}
import scala.util.{Failure, Success}

case class CliConnections(
    `tcp-listen-port`: Option[Int],
    `tcp-connect`: List[(String, Int)],
    `webserver-listen-port`: Option[Int],
    `webserver-static-path`: Option[Path],
    `northwind-path`: Option[Path],
    //    `random-data-time`: Argument[Long, Option, Style.Named] =
    //      Argument(_.text("add random data on a time").valueName("milliseconds"))
)

class FbdcCli(settings: CliConnections) {

  val exData = new FbdcExampleData()

  val executor             = Executors.newCachedThreadPool()
  val ec: ExecutionContext = ExecutionContext.fromExecutor(executor)

  def start() =
    settings.`tcp-listen-port` match
      case None =>
      case Some(port) =>
        exData.dataManager.addLatentConnection(TCP.listen("0", port, ec))
    settings.`webserver-listen-port` match
      case None =>
      case Some(port) =>
        val server = new JettyServer(settings.`webserver-static-path`, "/", exData.dataManager, "0")
        server.start(port)
    settings.`tcp-connect`.collect {
      case (ip, port) =>
        (ip.trim, port)
    }.foreach: (ip, port) =>
      exData.dataManager.addLatentConnection(TCP.connect(ip, port, ec))
    settings.`northwind-path` match
      case None    =>
      case Some(p) => Northwind.enableConditional(exData, p)
    Fortunes.enableConditional(exData)
  end start

}
