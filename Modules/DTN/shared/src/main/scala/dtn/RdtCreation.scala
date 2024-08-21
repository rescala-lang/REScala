package dtn

import _root_.replication.DataManager
import rdts.base.LocalUid
import dtn.rdt.Channel
import _root_.replication.JsoniterCodecs.given
import com.github.plokhotnyuk.jsoniter_scala.core.JsonValueCodec
import com.github.plokhotnyuk.jsoniter_scala.macros.JsonCodecMaker
import com.github.plokhotnyuk.jsoniter_scala.macros.CodecMakerConfig
import dtn.rdt.ClientOperationMode

trait CaseStudyRdt {
  def connect(
      host: String,
      port: Int,
      monitoringClient: MonitoringClientInterface,
      operationMode: ClientOperationMode
  ): Unit
  def caseStudyListen(): Unit
  def caseStudyActive(): Unit
}

class AddWinsSetRDT(number_of_additions: Int, sleep_time_milliseconds: Long) extends CaseStudyRdt {
  type RdtType = Set[String]

  given JsonValueCodec[RdtType] = JsonCodecMaker.make(CodecMakerConfig.withMapAsArray(true))

  val dataManager: DataManager[RdtType] = DataManager[RdtType](
    LocalUid.gen(),
    state => println("replica received new state information"),
    _ => ()
  )

  def connect(
      host: String,
      port: Int,
      monitoringClient: MonitoringClientInterface,
      operationMode: ClientOperationMode
  ): Unit = {
    dataManager.addLatentConnection(Channel[RdtType](
      host,
      port,
      "app1",
      scala.concurrent.ExecutionContext.global,
      monitoringClient,
      operationMode
    ))
  }

  def caseStudyListen(): Unit = {
    while true do {
      Thread.sleep(1000)
    }
  }

  def caseStudyActive(): Unit = {
    println("started active add-wins rdt.")
    println(s"\nnumber of additions: ${number_of_additions}\nsleep-time milliseconds: ${sleep_time_milliseconds}")

    Thread.sleep(10 * 1000)

    for i <- 0 to number_of_additions do {
      Thread.sleep(sleep_time_milliseconds)
      dataManager.applyUnrelatedDelta(Set(s"hello world ${i} from ${dataManager.replicaId}"))
    }

    while true do {
      Thread.sleep(1000)
    }
  }
}
