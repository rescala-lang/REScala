package rescala.extra.lattices.delta.crdt

import com.github.plokhotnyuk.jsoniter_scala.core.JsonValueCodec
import com.github.plokhotnyuk.jsoniter_scala.macros.{CodecMakerConfig, JsonCodecMaker}
import rescala.extra.lattices.delta.DeltaCRDT._
import rescala.extra.lattices.delta.DotStore.DotFun
import rescala.extra.lattices.delta._
import rescala.extra.lattices.delta.crdt.MVRegisterCRDT.State

object MVRegisterCRDT {
  type State[A, C] = Causal[DotFun[A], C]

  def apply[A: UIJDLattice, C: CContext](antiEntropy: AntiEntropy[State[A, C]]): DeltaCRDT[State[A, C]] =
    DeltaCRDT.empty[State[A, C]](antiEntropy)

  def read[A, C: CContext]: DeltaQuery[State[A, C], Set[A]] = {
    case Causal(df, _) => df.values.toSet
  }

  def write[A: UIJDLattice, C: CContext](v: A): DeltaMutator[State[A, C]] = {
    case (replicaID, Causal(df, cc)) =>
      val nextDot = CContext[C].nextDot(cc, replicaID)

      Causal(
        Map(nextDot -> v),
        CContext[C].fromSet(df.keySet + nextDot)
      )
  }

  def clear[A: UIJDLattice, C: CContext]: DeltaMutator[State[A, C]] = {
    case (_, Causal(df, _)) =>
      Causal(
        DotFun[A].empty,
        CContext[C].fromSet(df.keySet)
      )
  }
}

class MVRegister[A: UIJDLattice, C: CContext](crdt: DeltaCRDT[MVRegisterCRDT.State[A, C]]) {
  def read: Set[A] = crdt.query(MVRegisterCRDT.read)

  def write(v: A): MVRegister[A, C] = new MVRegister(crdt.mutate(MVRegisterCRDT.write(v)))

  def clear(): MVRegister[A, C] = new MVRegister(crdt.mutate(MVRegisterCRDT.clear))

  def processReceivedDeltas(): MVRegister[A, C] = new MVRegister(crdt.processReceivedDeltas())
}

object MVRegister {
  type State[A, C] = MVRegisterCRDT.State[A, C]
  type Embedded[A] = DotFun[A]

  def apply[A: UIJDLattice, C: CContext](antiEntropy: AntiEntropy[State[A, C]]): MVRegister[A, C] =
    new MVRegister(MVRegisterCRDT[A, C](antiEntropy))

  implicit def MVRegisterStateCodec[A: JsonValueCodec, C: JsonValueCodec]: JsonValueCodec[Causal[Map[Dot, A], C]] =
    JsonCodecMaker.make(CodecMakerConfig.withMapAsArray(true))

  implicit def MVRegisterEmbeddedCodec[A: JsonValueCodec]: JsonValueCodec[Map[Dot, A]] =
    JsonCodecMaker.make(CodecMakerConfig.withMapAsArray(true))
}

class RMVRegister[A: UIJDLattice, C: CContext](val crdt: RDeltaCRDT[MVRegisterCRDT.State[A, C]]) extends CRDTInterface[MVRegisterCRDT.State[A, C]] {
  def read: Set[A] = crdt.query(MVRegisterCRDT.read)

  def write(v: A): RMVRegister[A, C] = new RMVRegister(crdt.mutate(MVRegisterCRDT.write(v)))

  def clear(): RMVRegister[A, C] = new RMVRegister(crdt.mutate(MVRegisterCRDT.clear))

  def applyDelta(delta: Delta[State[A, C]]): CRDTInterface[State[A, C]] = {
    val newCRDT = crdt.applyDelta(delta)
    if (newCRDT == crdt) this else new RMVRegister(newCRDT)
  }
}

object RMVRegister {
  type State[A, C] = MVRegisterCRDT.State[A, C]
  type Embedded[A] = DotFun[A]

  def apply[A: UIJDLattice, C: CContext](replicaID: String): RMVRegister[A, C] =
    new RMVRegister(RDeltaCRDT.empty[State[A, C]](replicaID))

  implicit def MVRegisterStateCodec[A: JsonValueCodec, C: JsonValueCodec]: JsonValueCodec[Causal[Map[Dot, A], C]] =
    JsonCodecMaker.make(CodecMakerConfig.withMapAsArray(true))

  implicit def MVRegisterEmbeddedCodec[A: JsonValueCodec]: JsonValueCodec[Map[Dot, A]] =
    JsonCodecMaker.make(CodecMakerConfig.withMapAsArray(true))

  def AtomicUIJDLattice[A]: UIJDLattice[A] = new UIJDLattice[A] {
    override def leq(left: A, right: A): Boolean = false

    /**
      * Decomposes a lattice state into its unique irredundant join decomposition of join-irreducable states
      */
    override def decompose(state: A): Set[A] = Set(state)

    override def bottom: A = throw new UnsupportedOperationException("Can't compute bottom of atomic type A")

    /** By assumption: associative, commutative, idempotent. */
    override def merge(left: A, right: A): A =
      if (left == right) {
        left
      } else {
        throw new UnsupportedOperationException(s"Can't merge atomic type A, left: $left, right: $right")
      }
  }
}
