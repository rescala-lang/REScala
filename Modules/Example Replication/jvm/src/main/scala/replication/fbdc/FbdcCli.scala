package replication.fbdc

import loci.communicator.tcp.TCP

import java.nio.file.Path

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
  import exData.registry

  def start() =
    settings.`tcp-listen-port` match
      case None =>
      case Some(port) =>
        registry.listen(TCP(port))
    settings.`webserver-listen-port` match
      case None =>
      case Some(port) =>
        val server = new JettyServer(settings.`webserver-static-path`, "/", exData.dataManager, "0")
        server.start(port)
    settings.`tcp-connect`.collect {
      case (ip, port) =>
        (ip.trim, port)
    }.foreach((ip, port) => registry.connect(TCP(ip, port)))
    settings.`northwind-path` match
      case None    =>
      case Some(p) => Northwind.enableConditional(exData, p)
    Fortunes.enableConditional(exData)
  end start

}
