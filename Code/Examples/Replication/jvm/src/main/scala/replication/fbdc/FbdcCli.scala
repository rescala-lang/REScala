package replication.fbdc

import com.github.plokhotnyuk.jsoniter_scala.core.JsonValueCodec
import com.github.plokhotnyuk.jsoniter_scala.macros.{CodecMakerConfig, JsonCodecMaker}
import de.rmgk.options.{Argument, Style}
import de.rmgk.script.extensions
import kofre.base.{Bottom, Id, Lattice}
import kofre.datatypes.alternatives.ObserveRemoveSet
import kofre.datatypes.{AddWinsSet, CausalQueue, ObserveRemoveMap, ReplicatedList}
import kofre.dotted.{Dotted, DottedLattice, HasDots}
import kofre.syntax.{PermCausalMutate, PermId}
import kofre.time.Dots
import loci.communicator.tcp.TCP
import loci.registry.Registry
import replication.DataManager
import replication.JsoniterCodecs.given

import java.nio.file.{Files, LinkOption, Path}
import java.util.Timer
import scala.annotation.nowarn

case class CliConnections(
    `tcp-listen-port`: Argument[Int, Option, Style.Named] = Argument(_.text("tcp listen port")),
    `tcp-connect`: Argument[String, List, Style.Named] = Argument(_.text("connections").valueName("<ip:port>")),
    `webserver-listen-port`: Argument[Int, Option, Style.Named] = Argument(_.text("webserver listen port")),
    `webserver-static-path`: Argument[Path, Option, Style.Named] = Argument(_.text("webserver static path")),
    `northwind-path`: Argument[Path, Option, Style.Named] = Argument(_.text("northwind sqlite database path"))
//    `random-data-time`: Argument[Long, Option, Style.Named] =
//      Argument(_.text("add random data on a time").valueName("milliseconds"))
)

class FbdcCli(settings: CliConnections) {

  val exData = new FbdcExampleData()
  import exData.{State, dataManager, registry}

  def start() =
    settings.`tcp-listen-port`.value match
      case None =>
      case Some(port) =>
        registry.listen(TCP(port))
    settings.`webserver-listen-port`.value match
      case None =>
      case Some(port) =>
        val server = new JettyServer(settings.`webserver-static-path`.value, "/", registry, "0")
        server.start(port)
    settings.`tcp-connect`.value.map { _.split(':') }.collect {
      case Array(ip, port) =>
        (ip.trim, Integer.parseInt(port))
    }.foreach((ip, port) => registry.connect(TCP(ip, port)))
    settings.`northwind-path`.value match
      case None    =>
      case Some(p) => Northwind.enableConditional(exData, p)
    Fortunes.enableConditional(exData)
  end start

}
