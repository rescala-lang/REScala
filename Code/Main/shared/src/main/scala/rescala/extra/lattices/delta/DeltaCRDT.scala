package rescala.extra.lattices.delta

import rescala.extra.lattices.delta.DeltaCRDT._
import rescala.extra.lattices.delta.DotStore._

case class DeltaCRDT[D: DotStore, C: CContext](replicaID: String, state: D, cc: C, deltaBuffer: List[CausalDelta[D, C]]) {
  def applyDelta[A: CContext](delta: CausalDelta[D, A]): DeltaCRDT[D, C] = delta match {
    case CausalDelta(origin, deltaState, deltaCC) =>
      DotStoreAsUIJDLattice[D, C].diff((state, cc), (deltaState, CContext[A].convert[C](deltaCC))) match {
        case Some((stateDiff, ccDiff)) =>
          val (stateMerged, ccMerged) = DotStore[D].merge(state, cc, stateDiff, ccDiff)
          val newBuffer = deltaBuffer :+ CausalDelta(origin, stateDiff, ccDiff)
          DeltaCRDT(replicaID, stateMerged, ccMerged, newBuffer)
        case None => this
      }
  }

  def handleSetDelta(delta: SetDelta[D]): DeltaCRDT[D, C] = delta match {
    case SetDelta(state, dots) => applyDelta(CausalDelta(replicaID, state, CContext[C].fromSet(dots)))
  }

  def joinedDeltaBuffer(target: String): CausalDelta[D, C] = deltaBuffer.filter {
    case CausalDelta(origin, _, _) => origin != target
  }.fold(CausalDelta(replicaID, DotStore[D].bottom, CContext[C].empty)) {
    case (CausalDelta(_, flagLeft, ccLeft), CausalDelta(_, flagRight, ccRight)) =>
      val (flagMerged, ccMerged) = DotStore[D].merge(flagLeft, ccLeft, flagRight, ccRight)
      CausalDelta(replicaID, flagMerged, ccMerged)
  }

  def query[A](q: DeltaQuery[D, A]): A = q(state)

  def mutate(m: DeltaDotMutator[D]): DeltaCRDT[D, C] = handleSetDelta(m(state, CContext[C].nextDot(cc, replicaID)))

  def mutate(m: DeltaMutator[D]): DeltaCRDT[D, C] = handleSetDelta(m(state))
}

object DeltaCRDT {
  type DeltaDotMutator[D] = (D, Dot) => SetDelta[D]
  type DeltaMutator[D] = D => SetDelta[D]
  type DeltaQuery[D, A] = D => A
}
