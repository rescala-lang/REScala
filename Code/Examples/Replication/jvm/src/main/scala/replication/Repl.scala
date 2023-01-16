package replication

import com.github.plokhotnyuk.jsoniter_scala.core.JsonValueCodec
import com.github.plokhotnyuk.jsoniter_scala.macros.{CodecMakerConfig, JsonCodecMaker}
import kofre.base.Id
import kofre.datatypes.ReplicatedList
import loci.registry.Registry
import replication.JsoniterCodecs.given

import scala.annotation.nowarn

import java.util.Timer

class Commandline(settings: CliConnections) {

  val replicaId         = Id.gen()
  val registry          = new Registry
  val connectionManager = new ConnectionManager(registry)

  val dataManager =
    @nowarn given JsonValueCodec[ReplicatedList[String]] = JsonCodecMaker.make(CodecMakerConfig.withMapAsArray(true))
    new DataManager[ReplicatedList[String]](replicaId, registry)

  val timer = new Timer()

  def start() =
    settings.`tcp-listen-port`.value match
      case None       =>
      case Some(port) => connectionManager.listenTcp(port)
    settings.`webserver-listen-port`.value match
      case None       =>
      case Some(port) => connectionManager.startWebserver(port)
    settings.`tcp-connect`.value.map { _.split(':') }.collect {
      case Array(ip, port) =>
        (ip.trim, Integer.parseInt(port))
    }.foreach(connectionManager.connectTcp.tupled)

  var count = 0

  def add() =
    dataManager.applyLocalDelta(dataManager.currentValue.append(s"${replicaId}: $count").anon)
    dataManager.disseminate()
    count = count + 1

  def read() = dataManager.currentValue.toList

}
