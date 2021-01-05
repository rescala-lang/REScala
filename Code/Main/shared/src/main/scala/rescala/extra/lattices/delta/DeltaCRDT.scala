package rescala.extra.lattices.delta

import rescala.extra.lattices.delta.DeltaCRDT._

case class DeltaCRDT[A: UIJDLattice](replicaID: String, state: A, deltaBuffer: List[Delta[A]]) {
  def applyDelta(delta: Delta[A]): DeltaCRDT[A] = delta match {
    case Delta(origin, deltaState) =>
      UIJDLattice[A].diff(state, deltaState) match {
        case Some(stateDiff) =>
          val stateMerged = UIJDLattice[A].merge(state, stateDiff)
          val newBuffer = deltaBuffer :+ Delta(origin, stateDiff)
          DeltaCRDT(replicaID, stateMerged, newBuffer)
        case None => this
      }
  }

  def joinedDeltaBuffer(target: String): Option[Delta[A]] = deltaBuffer.filter {
    case Delta(origin, _) => origin != target
  }.reduceOption { (left: Delta[A], right: Delta[A]) =>
    val stateMerged = UIJDLattice[A].merge(left.deltaState, right.deltaState)
    Delta(replicaID, stateMerged)
  }

  def receiveDelta(other: DeltaCRDT[A]): DeltaCRDT[A] = other.joinedDeltaBuffer(replicaID) match {
    case Some(delta) =>
      applyDelta(delta)
    case None => this
  }

  def query[B](q: DeltaQuery[A, B]): B = q(state)

  def mutate(m: DeltaMutator[A]): DeltaCRDT[A] = applyDelta(m(replicaID, state))
}

object DeltaCRDT {
  type DeltaMutator[A] = (String, A) => Delta[A]
  type DeltaQuery[A, B] = A => B
}
