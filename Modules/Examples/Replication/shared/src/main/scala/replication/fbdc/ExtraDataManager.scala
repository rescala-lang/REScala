package replication.fbdc

import com.github.plokhotnyuk.jsoniter_scala.core.{JsonValueCodec, writeToArray}
import com.github.plokhotnyuk.jsoniter_scala.macros.JsonCodecMaker
import rdts.base.{Bottom, Lattice, Uid}
import rdts.time.Dots
import reactives.operator.{Event, Signal, Var}
import replication.JsoniterCodecs.given
import replication.{DeltaDissemination, ProtocolDots}

import scala.collection.mutable

class ExtraDataManager[St](val dataManager: DeltaDissemination[St], changeEvt: Event[ProtocolDots[St]])(using
    jsonCodec: JsonValueCodec[St],
    lattice: Lattice[St],
    bottom: Bottom[St]
) {

  given JsonValueCodec[ProtocolDots[St]] = JsonCodecMaker.make

  export dataManager.{selfContext as _, *}

  val changes: Event[TransferState] = changeEvt
  val mergedState                   = changes.fold(Bottom.empty[ProtocolDots[St]]) { (curr, ts) => curr `merge` ts }
  val currentContext: Signal[Dots]  = mergedState.map(_.context)

  def transform(fun: St => St) = dataManager.lock.synchronized {
    val current = mergedState.now
    dataManager.applyLocalDelta(ProtocolDots(
      fun(current.data),
      Dots.single(current.context.nextDot(dataManager.replicaId.uid))
    ))
  }

  val encodedStateSize = mergedState.map(s => writeToArray(s).size)

  private val contexts: Var[Map[Uid, Dots]]                            = Var(Map.empty)
  private val filteredContexts: mutable.Map[Uid, Signal[Option[Dots]]] = mutable.Map.empty

  def peerids: Signal[Set[Uid]] = contexts.map(_.keySet)
  def contextOf(rr: Uid): Signal[Dots] = Signal {
    contexts.value.getOrElse(rr, Dots.empty)
  }

}
