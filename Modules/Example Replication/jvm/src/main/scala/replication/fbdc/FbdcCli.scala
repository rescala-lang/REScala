package replication.fbdc

import channel.BiChan
import channel.tcp.{TCPConnection}

import java.nio.file.Path
import scala.concurrent.Future
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

  def start() =
    settings.`tcp-listen-port` match
      case None =>
      case Some(port) =>
//        val listening = TCPListener.startListening(port, "0")
//        new Thread(() =>
//          listening.connections.run(using ()):
//            case Success(conn) => exData.dataManager.addConnection(BiChan(conn, conn))
//            case Failure(ex)   => ex.printStackTrace()
//        ).start()
    settings.`webserver-listen-port` match
      case None =>
      case Some(port) =>
        val server = new JettyServer(settings.`webserver-static-path`, "/", exData.dataManager, "0")
        server.start(port)
    settings.`tcp-connect`.collect {
      case (ip, port) =>
        (ip.trim, port)
    }.foreach: (ip, port) =>
      // TODO: the added connection here will hijack whatever thread calls .receive which is really not what one would want in this case
//      channel.tcp.connect(ip, port).run(using ()):
//        case Success(conn) => exData.dataManager.addConnection(BiChan(conn, conn))
//        case Failure(ex)   => ex.printStackTrace()
      ()
    settings.`northwind-path` match
      case None    =>
      case Some(p) => Northwind.enableConditional(exData, p)
    Fortunes.enableConditional(exData)
  end start

}
