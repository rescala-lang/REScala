package dtn

import _root_.replication.DataManager
import rdts.base.LocalUid
import dtn.rdt.Channel
import _root_.replication.JsoniterCodecs.given
import com.github.plokhotnyuk.jsoniter_scala.core.JsonValueCodec
import com.github.plokhotnyuk.jsoniter_scala.macros.JsonCodecMaker
import com.github.plokhotnyuk.jsoniter_scala.macros.CodecMakerConfig

class AddWinsSetRDT {
  given JsonValueCodec[Set[String]] = JsonCodecMaker.make(CodecMakerConfig.withMapAsArray(true))

  val dataManager: DataManager[Set[String]] = DataManager[Set[String]](
    LocalUid.gen(),
    state => println("replica received new state information"),
    _ => ()
  )

  def connect(host: String, port: Int, monitoringClient: MonitoringClientInterface): Unit = {
    dataManager.addLatentConnection(Channel[Set[String]](
      host,
      port,
      "app1",
      scala.concurrent.ExecutionContext.global,
      monitoringClient
    ))
  }

  def caseStudyListen(): Unit = {
    while true do {
      Thread.sleep(1000)
    }
  }

  def caseStudyActive(): Unit = {
    Thread.sleep(10 * 1000)

    for i <- 0 to 2000 do {
      Thread.sleep(500)
      dataManager.applyUnrelatedDelta(Set(s"hello world ${i} from ${dataManager.replicaId}"))
    }
  }
}
