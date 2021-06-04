package rescala.extra.lattices.delta.impl.basic

import rescala.extra.lattices.delta.{Delta, DeltaMutator, DeltaQuery, UIJDLattice}

case class DeltaCRDT[A: UIJDLattice](state: A, antiEntropy: AntiEntropy[A]) {
  private val replicaID: String = antiEntropy.replicaID

  def query[B](q: DeltaQuery[A, B]): B = q(state)

  def mutate(m: DeltaMutator[A]): DeltaCRDT[A] = applyDelta(Delta(replicaID, m(replicaID, state)))

  private def applyDelta(delta: Delta[A]): DeltaCRDT[A] = delta match {
    case Delta(origin, deltaState) =>
      UIJDLattice[A].diff(state, deltaState) match {
        case Some(stateDiff) =>
          val stateMerged = UIJDLattice[A].merge(state, stateDiff)
          antiEntropy.recordChange(Delta(origin, stateDiff), stateMerged)
          this.copy(state = stateMerged)
        case None => this
      }
  }

  def processReceivedDeltas(): DeltaCRDT[A] = antiEntropy.getReceivedDeltas.foldLeft(this) {
    (crdt, delta) => crdt.applyDelta(delta)
  }
}

case object DeltaCRDT {
  def empty[A: UIJDLattice](antiEntropy: AntiEntropy[A]): DeltaCRDT[A] =
    DeltaCRDT[A](UIJDLattice[A].bottom, antiEntropy)
}
