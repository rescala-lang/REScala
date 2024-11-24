package dtn

import _root_.replication.{DeltaDissemination, ProtocolDots}
import rdts.base.LocalUid
import dtn.rdt.Channel
import _root_.replication.JsoniterCodecs.given
import com.github.plokhotnyuk.jsoniter_scala.core.JsonValueCodec
import com.github.plokhotnyuk.jsoniter_scala.macros.JsonCodecMaker
import com.github.plokhotnyuk.jsoniter_scala.macros.CodecMakerConfig
import dtn.rdt.ClientOperationMode
import rdts.datatypes.alternatives.ObserveRemoveSet
import scala.util.Random
import rdts.dotted.Obrem
import rdts.datatypes.contextual.ObserveRemoveMap
import rdts.time.Dot
import rdts.base.Lattice
import rdts.datatypes.LastWriterWins
import rdts.time.Dots

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

  val dataManager: DeltaDissemination[RdtType] = DeltaDissemination[RdtType](
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

    println("finshed adding changes")

    while true do {
      Thread.sleep(1000)
    }
  }
}

class ObserveRemoveSetRDT(number_of_changes: Int, sleep_time_milliseconds: Long) extends CaseStudyRdt {
  type RdtType = Obrem[ObserveRemoveMap[String, Dot]]

  given JsonValueCodec[RdtType] = JsonCodecMaker.make(CodecMakerConfig.withMapAsArray(true))

  given Lattice[Dot] = Lattice.assertEquals

  given replicaId: LocalUid = LocalUid.gen()

  val dataManager: DeltaDissemination[RdtType] = DeltaDissemination[RdtType](
    replicaId,
    state =>
      println("replica received new state information"), // we ignore state updates as there will be only one active rdt
    _ => ()
  )

  var state: RdtType = Obrem(ObserveRemoveMap.empty[String, Dot])

  private def addStringGetDelta(s: String): RdtType = {
    state.mod { ctx ?=> current =>
      val nextDot = ctx.nextDot(dataManager.replicaId.uid)
      current.update(s, nextDot)
    }
  }

  private def removeStringGetDelta(s: String): RdtType = {
    state.mod { ctx ?=> current =>
      current.remove(s)
    }
  }

  private def clearGetDelta(): RdtType = {
    state.mod { ctx ?=> current =>
      current.clear()
    }
  }

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
    println("started active observe-remove-set rdt.")
    println(s"\nnumber of changes: ${number_of_changes}\nsleep-time milliseconds: ${sleep_time_milliseconds}")

    Thread.sleep(10 * 1000)

    for i <- 0 to number_of_changes do {
      Thread.sleep(sleep_time_milliseconds)

      var delta = addStringGetDelta(s"hello world ${i} from ${dataManager.replicaId}")
      state = state.merge(delta)

      if i > 0 && i % 100 == 0 then {
        if Random().nextBoolean() then {
          delta = delta.merge(clearGetDelta())
          state = state.merge(delta)
        }

        /*
        for j <- i - 10 to Random().between(i - 10, i) do {
          delta = delta.merge(removeStringGetDelta(s"hello world ${j} from ${dataManager.replicaId}"))
          state = state.merge(delta)
        }
         */
      }

      dataManager.applyLocalDelta(ProtocolDots(delta, delta.context))
    }

    println("finshed adding changes")

    while true do {
      Thread.sleep(1000)
    }
  }
}

class LastWriterWinsRDT(number_of_changes: Int, sleep_time_milliseconds: Long) extends CaseStudyRdt {
  type RdtType = LastWriterWins[Set[String]]

  given JsonValueCodec[RdtType] = JsonCodecMaker.make(CodecMakerConfig.withMapAsArray(true))

  given replicaId: LocalUid = LocalUid.gen()

  val dataManager: DeltaDissemination[RdtType] = DeltaDissemination[RdtType](
    replicaId,
    state => println("replica received new state information"),
    _ => ()
  )

  var state = LastWriterWins.empty[Set[String]]
  var dots  = Dots.empty

  private def writeStringGetDeltaInfo(s: String): Tuple2[RdtType, Dots] = {
    state = state.write(Set(s))         // advances a total ordering internally
    dots = dots.advanced(replicaId.uid) // advances a total ordering externally for the metadata

    (state, dots)
  }

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
    println("started active last-writer-wins rdt.")
    println(s"\nnumber of changes: ${number_of_changes}\nsleep-time milliseconds: ${sleep_time_milliseconds}")

    Thread.sleep(10 * 1000)

    for i <- 0 to number_of_changes do {
      Thread.sleep(sleep_time_milliseconds)

      val (state, dots) = writeStringGetDeltaInfo(s"hello world ${i} from ${dataManager.replicaId}")

      dataManager.applyLocalDelta(ProtocolDots(state, dots))
    }

    while true do {
      Thread.sleep(1000)
    }
  }
}
