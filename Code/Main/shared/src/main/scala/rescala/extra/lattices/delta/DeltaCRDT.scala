package rescala.extra.lattices.delta

import rescala.extra.lattices.delta.DeltaCRDT._

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
  type DeltaMutator[A]  = (String, A) => A
  type DeltaQuery[A, B] = A => B

  def empty[A: UIJDLattice](antiEntropy: AntiEntropy[A]): DeltaCRDT[A] =
    DeltaCRDT[A](UIJDLattice[A].bottom, antiEntropy)
}

case class RDeltaCRDT[A: UIJDLattice](state: A, replicaID: String, lastDelta: Option[Delta[A]] = None) {
  def query[B](q: DeltaQuery[A, B]): B = q(state)

  def mutate(m: DeltaMutator[A]): RDeltaCRDT[A] = applyDelta(Delta(replicaID, m(replicaID, state)))

  def applyDelta(delta: Delta[A]): RDeltaCRDT[A] = delta match {
    case Delta(origin, deltaState) =>
      UIJDLattice[A].diff(state, deltaState) match {
        case Some(stateDiff) =>
          val stateMerged = UIJDLattice[A].merge(state, stateDiff)
          this.copy(state = stateMerged, lastDelta = Some(Delta(origin, stateDiff)))
        case None => this
      }
  }
}

case object RDeltaCRDT {
  type DeltaMutator[A]  = (String, A) => A
  type DeltaQuery[A, B] = A => B

  def empty[A: UIJDLattice](replicaID: String): RDeltaCRDT[A] =
    RDeltaCRDT[A](UIJDLattice[A].bottom, replicaID)
}
