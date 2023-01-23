package replication

import com.github.plokhotnyuk.jsoniter_scala.core.JsonValueCodec
import com.github.plokhotnyuk.jsoniter_scala.macros.{CodecMakerConfig, JsonCodecMaker}
import kofre.base.Id
import kofre.datatypes.ReplicatedList
import loci.communicator.tcp.TCP
import loci.registry.Registry
import replication.JsoniterCodecs.given
import replication.server.JettyServer

import scala.annotation.nowarn
import java.util.Timer

class Commandline(settings: CliConnections) {

  val replicaId = Id.gen()
  val registry  = new Registry

  val dataManager =
    @nowarn given JsonValueCodec[ReplicatedList[String]] = JsonCodecMaker.make(CodecMakerConfig.withMapAsArray(true))
    new DataManager[ReplicatedList[String]](replicaId, registry)

  val timer = new Timer()

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
    settings.`random-data-time`.value match
      case None     =>
      case Some(ms) => timer.scheduleAtFixedRate(() => add(), 0, ms)

  var count = 0

  def add() =
    dataManager.applyLocalDelta(dataManager.currentValue.append(s"${Id.unwrap(replicaId).take(4)}: $count").anon)
    dataManager.disseminate()
    count = count + 1

  def read() = dataManager.currentValue.toList

}
