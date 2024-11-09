package replication.fbdc

import channels.TCP
import com.github.plokhotnyuk.jsoniter_scala.core.JsonValueCodec
import com.github.plokhotnyuk.jsoniter_scala.macros.JsonCodecMaker
import replication.JsoniterCodecs.given

import java.net.{InetSocketAddress, Socket}
import java.nio.file.Path
import java.util.concurrent.Executors
import scala.concurrent.ExecutionContext

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

  given jsonCodec: JsonValueCodec[State] = JsonCodecMaker.make

  val executor             = Executors.newCachedThreadPool()
  val ec: ExecutionContext = ExecutionContext.fromExecutor(executor)

  def start() =
    settings.`tcp-listen-port` match
      case None =>
      case Some(port) =>
        exData.dataManager.addLatentConnection(TCP.listen(TCP.defaultSocket(InetSocketAddress("0", port)), ec))
    settings.`webserver-listen-port` match
      case None =>
      case Some(port) =>
        val server = new JettyServer(settings.`webserver-static-path`, "/", exData.dataManager.dataManager, "0")
        server.start(port)
    settings.`tcp-connect`.collect {
      case (ip, port) =>
        (ip.trim, port)
    }.foreach: (ip, port) =>
      exData.dataManager.addLatentConnection(TCP.connect(() => Socket(ip, port), ec))
    settings.`northwind-path` match
      case None    =>
      case Some(p) => Northwind.enableConditional(exData, p)
    Fortunes.enableConditional(exData)
  end start

}
