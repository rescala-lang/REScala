package rescala.extra.lattices.delta

import rescala.extra.lattices.delta.DeltaCRDT._

case class DeltaCRDT[A: UIJDLattice](replicaID: String, state: A, antiEntropy: AntiEntropy[A]) {
  def query[B](q: DeltaQuery[A, B]): B = q(state)

  def mutate(m: DeltaMutator[A]): DeltaCRDT[A] = applyDelta(Delta(replicaID, m(replicaID, state)))

  private def applyDelta(delta: Delta[A]): DeltaCRDT[A] = delta match {
    case Delta(origin, deltaState) =>
      UIJDLattice[A].diff(state, deltaState) match {
        case Some(stateDiff) =>
          antiEntropy.addToOutBuffer(Delta(origin, stateDiff))
          val stateMerged = UIJDLattice[A].merge(state, stateDiff)
          this.copy(state = stateMerged)
        case None => this
      }
  }

  def processReceivedDeltas(): DeltaCRDT[A] = antiEntropy.getReceivedDeltas.foldLeft(this) {
    (crdt, delta) => crdt.applyDelta(delta)
  }
}

case object DeltaCRDT {
  type DeltaMutator[A] = (String, A) => A
  type DeltaQuery[A, B] = A => B

  def empty[A: UIJDLattice](replicaID: String, antiEntropy: AntiEntropy[A]): DeltaCRDT[A] =
    DeltaCRDT[A](replicaID, UIJDLattice[A].bottom, antiEntropy)

  def empty[A: UIJDLattice](replicaID: String): DeltaCRDT[A] =
    DeltaCRDT[A](replicaID, UIJDLattice[A].bottom, new AntiEntropy[A](replicaID))
}
