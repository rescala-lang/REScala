package rescala.extra.lattices.delta

import rescala.extra.lattices.delta.DeltaCRDT._

case class DeltaCRDT[D: DotStore, C: CContext](replicaID: String, state: D, cc: C, deltaBuffer: List[CausalDelta[D, C]]) {
  def applyDelta[A: CContext](delta: CausalDelta[D, A], save: Boolean = false): DeltaCRDT[D, C] = delta match {
    case CausalDelta(deltaState, deltaCC) =>
      val (stateMerged, ccMerged) = DotStore[D].merge(state, cc, deltaState, deltaCC)
      if (save) {
        val newBuffer = deltaBuffer :+ CausalDelta(deltaState, CContext[A].convert[C](deltaCC))
        DeltaCRDT(replicaID, stateMerged, ccMerged, newBuffer)
      } else {
        DeltaCRDT(replicaID, stateMerged, ccMerged, deltaBuffer)
      }
  }

  def handleDelta(delta: SetDelta[D]): DeltaCRDT[D, C] = delta match {
    case SetDelta(deltaState, dots) => applyDelta(CausalDelta(deltaState, CContext[C].fromSet(dots)), save = true)
  }

  def joinedDeltaBuffer: CausalDelta[D, C] = deltaBuffer.fold(CausalDelta(DotStore[D].bottom, CContext[C].empty)) {
    case (CausalDelta(flagLeft, ccLeft), CausalDelta(flagRight, ccRight)) =>
      val (flagMerged, ccMerged) = DotStore[D].merge(flagLeft, ccLeft, flagRight, ccRight)
      CausalDelta(flagMerged, ccMerged)
  } match {
    case CausalDelta(flag, cc) => CausalDelta(flag, cc)
  }

  def query[A](q: DeltaQuery[D, A]): A = q(state)

  def mutate(m: DeltaDotMutator[D]): DeltaCRDT[D, C] = handleDelta(m(state, CContext[C].nextDot(cc, replicaID)))

  def mutate(m: DeltaMutator[D]): DeltaCRDT[D, C] = handleDelta(m(state))
}

object DeltaCRDT {
  type DeltaDotMutator[D] = (D, Dot) => SetDelta[D]
  type DeltaMutator[D] = D => SetDelta[D]
  type DeltaQuery[D, A] = D => A
}
