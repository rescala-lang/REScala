package rescala.extra.lattices.delta

import rescala.extra.lattices.delta.DeltaCRDT._

case class DeltaCRDT[A: UIJDLattice](
  replicaID: String,
  state: A,
  deltaBuffer: Map[Int, Delta[A]],
  seqNum: Int,
  ackMap: Map.WithDefault[String, Int]
) {

  def query[B](q: DeltaQuery[A, B]): B = q(state)

  def mutate(m: DeltaMutator[A]): DeltaCRDT[A] = applyDelta(Delta(replicaID, m(replicaID, state)))

  def applyDelta(delta: Delta[A]): DeltaCRDT[A] = delta match {
    case Delta(origin, deltaState) =>
      UIJDLattice[A].diff(state, deltaState) match {
        case Some(stateDiff) =>
          val stateMerged = UIJDLattice[A].merge(state, stateDiff)
          val newBuffer = deltaBuffer + (seqNum -> Delta(origin, stateDiff))
          this.copy(state = stateMerged, deltaBuffer = newBuffer, seqNum = seqNum + 1)
        case None => this
      }
  }

  def joinedDeltaBuffer(target: String): Option[Delta[A]] = deltaBuffer.values.filter {
    case Delta(origin, _) => origin != target
  }.reduceOption { (left: Delta[A], right: Delta[A]) =>
    val stateMerged = UIJDLattice[A].merge(left.deltaState, right.deltaState)
    Delta(replicaID, stateMerged)
  }

  def receiveDelta(delta: Delta[A]): DeltaCRDT[A] = {
    applyDelta(delta)
  }

  def recvAck(from: String, n: Int): DeltaCRDT[A] = {
    val maxAck = ackMap(from) max n

    this.copy(ackMap = ackMap.updated(from, maxAck))
  }

  def prepareDelta(to: String): Option[Delta[A]] = {
    if (deltaBuffer.isEmpty || deltaBuffer.keySet.min > ackMap(to))
      Some(Delta(replicaID, state))
    else {
      deltaBuffer.collect {
        case (n, deltaState) if ackMap(to) until seqNum contains n => deltaState
      } reduceOption { (left: Delta[A], right: Delta[A]) =>
        Delta(replicaID, UIJDLattice[A].merge(left.deltaState, right.deltaState))
      }
    }
  }

  def gc(): DeltaCRDT[A] = {
    val newBuffer = deltaBuffer.filter {
      case (n, _) => n >= ackMap.values.min
    }

    this.copy(deltaBuffer = newBuffer)
  }
}

case object DeltaCRDT {
  type DeltaMutator[A] = (String, A) => A
  type DeltaQuery[A, B] = A => B

  def empty[A: UIJDLattice](replicaID: String, state: A): DeltaCRDT[A] =
    DeltaCRDT[A](
      replicaID,
      state,
      Map(),
      0,
      new Map.WithDefault[String, Int](Map(), _ => -1))

  def empty[A: UIJDLatticeWithBottom](replicaID: String): DeltaCRDT[A] =
    empty(replicaID, UIJDLatticeWithBottom[A].bottom)
}
