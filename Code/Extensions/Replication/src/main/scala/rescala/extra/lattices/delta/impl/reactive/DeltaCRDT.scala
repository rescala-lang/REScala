package rescala.extra.lattices.delta.impl.reactive

import rescala.extra.lattices.delta.{DeltaMutator, DeltaQuery}
import rescala.extra.lattices.delta.{Delta, UIJDLattice}

case class DeltaCRDT[A: UIJDLattice](state: A, replicaID: String, deltaBuffer: List[Delta[A]] = List()) {
  def query[B](q: DeltaQuery[A, B]): B = q(state)

  def mutate(m: DeltaMutator[A]): DeltaCRDT[A] = applyDelta(Delta(replicaID, m(replicaID, state)))

  def applyDelta(delta: Delta[A]): DeltaCRDT[A] = delta match {
    case Delta(origin, deltaState) =>
      UIJDLattice[A].diff(state, deltaState) match {
        case Some(stateDiff) =>
          val stateMerged = UIJDLattice[A].merge(state, stateDiff)
          this.copy(state = stateMerged, deltaBuffer = Delta(origin, stateDiff) :: deltaBuffer)
        case None => this
      }
  }

  def resetDeltaBuffer(): DeltaCRDT[A] = this.copy(deltaBuffer = List())
}

case object DeltaCRDT {
  def empty[A: UIJDLattice](replicaID: String): DeltaCRDT[A] =
    DeltaCRDT[A](UIJDLattice[A].bottom, replicaID)
}
