package rescala.extra.lattices.delta.crdt

import com.github.plokhotnyuk.jsoniter_scala.core.JsonValueCodec
import com.github.plokhotnyuk.jsoniter_scala.macros.JsonCodecMaker
import rescala.extra.lattices.delta.DeltaCRDT._
import rescala.extra.lattices.delta.DotStore._
import rescala.extra.lattices.delta._

object EWFlagCRDT {
  type State[C] = Causal[DotSet, C]

  private def deltaState[C: CContext](
      ds: Option[DotSet] = None,
      cc: C
  ): State[C] = {
    val bottom = UIJDLattice[State[C]].bottom

    Causal(
      ds.getOrElse(bottom.dotStore),
      cc
    )
  }

  def read[C: CContext]: DeltaQuery[State[C], Boolean] = {
    case Causal(ds, _) => ds.nonEmpty
  }

  def enable[C: CContext](): DeltaMutator[State[C]] = {
    case (replicaID, Causal(ds, cc)) =>
      val nextDot = CContext[C].nextDot(cc, replicaID)

      deltaState(
        ds = Some(Set(nextDot)),
        cc = CContext[C].fromSet(ds + nextDot)
      )
  }

  def disable[C: CContext](): DeltaMutator[State[C]] = {
    case (_, Causal(ds, _)) =>
      deltaState(
        cc = CContext[C].fromSet(ds)
      )
  }
}

class EWFlag[C: CContext](crdt: DeltaCRDT[EWFlagCRDT.State[C]]) {
  def read: Boolean = crdt.query(EWFlagCRDT.read)

  def enable(): EWFlag[C] = new EWFlag(crdt.mutate(EWFlagCRDT.enable()))

  def disable(): EWFlag[C] = new EWFlag(crdt.mutate(EWFlagCRDT.disable()))

  def processReceivedDeltas(): EWFlag[C] = new EWFlag(crdt.processReceivedDeltas())
}

object EWFlag {
  type State[C] = EWFlagCRDT.State[C]
  type Embedded = DotSet

  def apply[C: CContext](antiEntropy: AntiEntropy[EWFlagCRDT.State[C]]): EWFlag[C] =
    new EWFlag(DeltaCRDT.empty(antiEntropy))

  implicit def EWFlagStateCodec[C: JsonValueCodec]: JsonValueCodec[Causal[Set[Dot], C]] = JsonCodecMaker.make

  //implicit def EWFlagEmbeddedCodec: JsonValueCodec[Set[Dot]] = JsonCodecMaker.make
}
