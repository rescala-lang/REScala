package rescala.extra.lattices.delta.crdt

import rescala.extra.lattices.delta.DeltaCRDT.{DeltaMutator, DeltaQuery}
import rescala.extra.lattices.delta.DotStore.DotFun
import rescala.extra.lattices.delta.{CContext, Causal, Delta, RDeltaCRDT, UIJDLattice}

object ORValueCRDT {
  type State[A, C] = Causal[DotFun[A], C]

  def read[A: UIJDLattice, C: CContext]: DeltaQuery[State[A, C], Option[A]] = {
    case Causal(df, _) => df.values.headOption
  }

  def write[A: UIJDLattice, C: CContext](v: A): DeltaMutator[State[A, C]] = {
    case (replicaID, Causal(df, cc)) =>
      val nextDot = CContext[C].nextDot(cc, replicaID)

      Causal(
        Map(nextDot -> v),
        CContext[C].fromSet(DotFun[A].dots(df))
      )
  }

  def clear[A: UIJDLattice, C: CContext](): DeltaMutator[State[A, C]] = {
    case (_, Causal(df, _)) =>
      Causal(
        DotFun[A].empty,
        CContext[C].fromSet(DotFun[A].dots(df))
      )
  }
}

object ORValue {
  type State[A, C] = ORValueCRDT.State[A, C]
  type Embedded[A] = DotFun[A]
}

case class TimedVal[A](value: A, timestamp: Long = System.currentTimeMillis())

object TimedVal {
  implicit def TimedValAsUIJDLattice[A]: UIJDLattice[TimedVal[A]] = new UIJDLattice[TimedVal[A]] {
    override def leq(left: TimedVal[A], right: TimedVal[A]): Boolean = left.timestamp <= right.timestamp

    /**
      * Decomposes a lattice state into its unique irredundant join decomposition of join-irreducable states
      */
    override def decompose(state: TimedVal[A]): Set[TimedVal[A]] = Set(state)

    override def bottom: TimedVal[A] = throw new UnsupportedOperationException("TimedVal does not have a bottom value")

    /** By assumption: associative, commutative, idempotent. */
    override def merge(left: TimedVal[A], right: TimedVal[A]): TimedVal[A] =
      if (left.timestamp > right.timestamp) left else right
  }
}

object LastWriterWinsCRDT {
  type State[A, C] = ORValueCRDT.State[TimedVal[A], C]

  def read[A, C: CContext]: DeltaQuery[State[A, C], Option[A]] = ORValueCRDT.read[TimedVal[A], C].andThen(_.map(tv => tv.value))

  def write[A, C: CContext](v: A): DeltaMutator[State[A, C]] = ORValueCRDT.write[TimedVal[A], C](TimedVal(value = v))

  def clear[A, C: CContext](): DeltaMutator[State[A, C]] = ORValueCRDT.clear[TimedVal[A], C]()
}

class LastWriterWins[A, C: CContext](val crdt: RDeltaCRDT[LastWriterWins.State[A, C]]) extends CRDTInterface[LastWriterWins.State[A, C]] {
  def read: Option[A] = crdt.query(LastWriterWinsCRDT.read)

  def write(v: A): LastWriterWins[A, C] = new LastWriterWins(crdt.mutate(LastWriterWinsCRDT.write(v)))

  def clear(): LastWriterWins[A, C] = new LastWriterWins(crdt.mutate(LastWriterWinsCRDT.clear()))

  def applyDelta(delta: Delta[LastWriterWins.State[A, C]]): LastWriterWins[A, C] = {
    val newCRDT = crdt.applyDelta(delta)
    if (newCRDT == crdt) this else new LastWriterWins(newCRDT)
  }
}

object LastWriterWins {
  type State[A, C] = LastWriterWinsCRDT.State[A, C]
  type Embedded[A] = DotFun[TimedVal[A]]

  def apply[A, C: CContext](replicaID: String): RDeltaCRDT[State[A, C]] =
    RDeltaCRDT.empty[State[A, C]](replicaID)
}
