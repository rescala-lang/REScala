package replication

import com.github.plokhotnyuk.jsoniter_scala.core.JsonValueCodec
import com.github.plokhotnyuk.jsoniter_scala.macros.{CodecMakerConfig, JsonCodecMaker}
import kofre.base.Id
import kofre.datatypes.ReplicatedList
import loci.registry.Registry
import replication.JsoniterCodecs.given

import scala.annotation.nowarn

class Repl {

  val registry = new Registry
  val connectionManager = new ConnectionManager(registry)

  val replicaId = Id.gen()

  @nowarn
  given JsonValueCodec[ReplicatedList[String]] = JsonCodecMaker.make(CodecMakerConfig.withMapAsArray(true))

  val dataManager = new DataManager[ReplicatedList[String]](replicaId, registry)

  var count = 0

  def host() = connectionManager.listenTcp(50443)
  def join() = connectionManager.connectTcp("127.0.0.1", 50443)

  def add() =
    dataManager.applyLocalDelta(dataManager.currentValue.append(s"${replicaId}: $count").anon)
    dataManager.disseminate()
    count = count + 1

  def read() = dataManager.currentValue.toList

}
