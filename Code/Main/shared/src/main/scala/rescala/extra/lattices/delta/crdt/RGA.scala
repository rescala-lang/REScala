package rescala.extra.lattices.delta.crdt

import com.github.plokhotnyuk.jsoniter_scala.core.JsonValueCodec
import com.github.plokhotnyuk.jsoniter_scala.macros.{CodecMakerConfig, JsonCodecMaker}
import rescala.extra.lattices.delta.DeltaCRDT.{DeltaMutator, DeltaQuery}
import rescala.extra.lattices.delta.DotStore.{DotFun, DotLess, DotPair}
import rescala.extra.lattices.delta._
import rescala.extra.lattices.delta.crdt.GOListCRDT.GOListAsUIJDLattice
import rescala.extra.lattices.delta.crdt.ForcedWriteCRDT.ForcedWriteAsUIJDLattice

import scala.annotation.tailrec

sealed trait RGANode[A]
case class Alive[A](v: TimedVal[A]) extends RGANode[A]
case class Dead[A]()                extends RGANode[A]

object RGANode {
  implicit def RGANodeAsUIJDLattice[A]: UIJDLattice[RGANode[A]] = new UIJDLattice[RGANode[A]] {
    override def leq(left: RGANode[A], right: RGANode[A]): Boolean = (left, right) match {
      case (Dead(), _)            => false
      case (_, Dead())            => true
      case (Alive(lv), Alive(rv)) => rv.laterThan(lv)
    }

    /** Decomposes a lattice state into its unique irredundant join decomposition of join-irreducible states */
    override def decompose(state: RGANode[A]): Set[RGANode[A]] = Set(state)

    override def bottom: RGANode[A] = throw new UnsupportedOperationException("RGANode does not have a bottom value")

    /** By assumption: associative, commutative, idempotent. */
    override def merge(left: RGANode[A], right: RGANode[A]): RGANode[A] = (left, right) match {
      case (Alive(lv), Alive(rv)) => Alive(UIJDLattice[TimedVal[A]].merge(lv, rv))
      case _                      => Dead()
    }
  }
}

object RGACRDT {
  type State[E, C] = Causal[DotPair[ForcedWrite.State[GOList.State[Dot]], DotFun[RGANode[E]]], C]

  implicit val ForcedWriteAsUIJDLattice: UIJDLattice[ForcedWrite.State[GOList.State[Dot]]] =
    ForcedWriteCRDT.ForcedWriteAsUIJDLattice[GOList.State[Dot]]

  private def readRec[E, C: CContext](state: State[E, C], target: Int, counted: Int, skipped: Int): Option[E] =
    (state: @unchecked) match {
      case Causal((FW(golist), df), _) =>
        GOListCRDT.read(counted + skipped)(golist).flatMap { d =>
          df(d) match {
            case Dead() => readRec(state, target, counted, skipped + 1)
            case Alive(tv) =>
              if (counted == target) Some(tv.value)
              else readRec(state, target, counted + 1, skipped)
          }
        }
    }

  def read[E, C: CContext](i: Int): DeltaQuery[State[E, C], Option[E]] = readRec(_, i, 0, 0)

  def size[E, C: CContext]: DeltaQuery[State[E, C], Int] = {
    case Causal((_, df), _) =>
      df.values.count {
        case Dead()   => false
        case Alive(_) => true
      }
  }

  def toList[E, C: CContext]: DeltaQuery[State[E, C], List[E]] = state =>
    (state: @unchecked) match {
      case Causal((FW(golist), df), _) =>
        GOListCRDT.toList(golist).flatMap(df.get).collect {
          case Alive(tv) => tv.value
        }
    }

  def sequence[E, C: CContext]: DeltaQuery[State[E, C], Long] = {
    case Causal(((c, _), _), _) => c
  }

  def insertRec[E, C: CContext](
      replicaID: String,
      state: State[E, C],
      target: Int,
      counted: Int,
      skipped: Int
  ): Option[ForcedWrite.State[GOList.State[Dot]]] =
    (state: @unchecked) match {
      case Causal((fw @ FW(golist), df), cc) =>
        if (target == counted) {
          val m = GOListCRDT.insert(counted + skipped, CContext[C].nextDot(cc, replicaID))
          Some(
            ForcedWriteCRDT.mutate(m)(replicaID, fw)
          )
        } else {
          GOListCRDT.read(counted + skipped)(golist) flatMap { d =>
            df(d) match {
              case Dead()   => insertRec(replicaID, state, target, counted, skipped + 1)
              case Alive(_) => insertRec(replicaID, state, target, counted + 1, skipped)
            }
          }
        }
    }

  def insert[E, C: CContext](i: Int, e: E): DeltaMutator[State[E, C]] = {
    case (replicaID, state @ Causal(_, cc)) =>
      val nextDot = CContext[C].nextDot(cc, replicaID)

      insertRec(replicaID, state, i, 0, 0) match {
        case None => UIJDLattice[State[E, C]].bottom
        case Some(golistDelta) =>
          val dfDelta = DotFun[RGANode[E]].empty + (nextDot -> Alive(TimedVal(e, replicaID)))

          Causal((golistDelta, dfDelta), CContext[C].fromSet(Set(nextDot)))
      }
  }

  def insertAllRec[E, C: CContext](
      replicaID: String,
      state: State[E, C],
      target: Int,
      counted: Int,
      skipped: Int,
      dots: Iterable[Dot]
  ): Option[ForcedWrite.State[GOList.State[Dot]]] =
    (state: @unchecked) match {
      case Causal((fw @ FW(golist), df), cc) =>
        if (target == counted) {
          val m = GOListCRDT.insertAll(counted + skipped, dots)
          Some(
            ForcedWriteCRDT.mutate(m)(replicaID, fw)
          )
        } else {
          GOListCRDT.read(counted + skipped)(golist) flatMap { d =>
            df(d) match {
              case Dead()   => insertAllRec(replicaID, state, target, counted, skipped + 1, dots)
              case Alive(_) => insertAllRec(replicaID, state, target, counted + 1, skipped, dots)
            }
          }
        }
    }

  def insertAll[E, C: CContext](i: Int, elems: Iterable[E]): DeltaMutator[State[E, C]] = {
    case (replicaID, state @ Causal(_, cc)) =>
      val nextDot = CContext[C].nextDot(cc, replicaID)

      val nextDots = List.iterate(nextDot, elems.size) {
        case Dot(c, r) => Dot(c + 1, r)
      }

      insertAllRec(replicaID, state, i, 0, 0, nextDots) match {
        case None => UIJDLattice[State[E, C]].bottom
        case Some(golistDelta) =>
          val dfDelta = DotFun[RGANode[E]].empty ++ (nextDots zip (elems.map(e => Alive(TimedVal(e, replicaID)))))

          Causal((golistDelta, dfDelta), CContext[C].fromSet(nextDots.toSet))
      }
  }

  @tailrec
  private def updateRGANodeRec[E, C: CContext](
      state: State[E, C],
      i: Int,
      newNode: RGANode[E],
      counted: Int,
      skipped: Int
  ): State[E, C] =
    (state: @unchecked) match {
      case Causal((FW(golist), df), _) =>
        GOListCRDT.read(counted + skipped)(golist) match {
          case None => UIJDLattice[State[E, C]].bottom
          case Some(d) =>
            df(d) match {
              case Dead() => updateRGANodeRec(state, i, newNode, counted, skipped + 1)
              case Alive(_) =>
                if (counted == i)
                  Causal(
                    (
                      UIJDLattice[ForcedWrite.State[GOList.State[Dot]]].bottom,
                      DotFun[RGANode[E]].empty + (d -> newNode)
                    ),
                    CContext[C].empty
                  )
                else
                  updateRGANodeRec(state, i, newNode, counted + 1, skipped)
            }
        }
    }

  private def updateRGANode[E, C: CContext](state: State[E, C], i: Int, newNode: RGANode[E]): State[E, C] =
    updateRGANodeRec(state, i, newNode, 0, 0)

  def update[E, C: CContext](i: Int, e: E): DeltaMutator[State[E, C]] =
    (replicaID, state) => updateRGANode(state, i, Alive(TimedVal(e, replicaID)))

  def delete[E, C: CContext](i: Int): DeltaMutator[State[E, C]] = (_, state) => updateRGANode(state, i, Dead[E]())

  private def updateRGANodeBy[E, C: CContext](
      state: State[E, C],
      cond: E => Boolean,
      newNode: RGANode[E]
  ): State[E, C] =
    (state: @unchecked) match {
      case Causal((_, df), _) =>
        val toUpdate = df.toList.collect {
          case (d, Alive(tv)) if cond(tv.value) => d
        }

        val dfDelta = DotFun[RGANode[E]].empty ++ toUpdate.map(_ -> newNode)

        Causal(
          (
            UIJDLattice[ForcedWrite.State[GOList.State[Dot]]].bottom,
            dfDelta
          ),
          CContext[C].empty
        )
    }

  def updateBy[E, C: CContext](cond: E => Boolean, e: E): DeltaMutator[State[E, C]] =
    (replicaID, state) => updateRGANodeBy(state, cond, Alive(TimedVal(e, replicaID)))

  def deleteBy[E, C: CContext](cond: E => Boolean): DeltaMutator[State[E, C]] =
    (_, state) => updateRGANodeBy(state, cond, Dead[E]())

  def purgeTombstones[E, C: CContext](): DeltaMutator[State[E, C]] = (replicaID, state) =>
    (state: @unchecked) match {
      case Causal((fw @ FW(golist), df), _) =>
        val toRemove = df.collect {
          case (dot, Dead()) => dot
        }.toSet

        val golistPurged = GOListCRDT.without(golist, toRemove)

        Causal(
          (ForcedWriteCRDT.forcedWrite(golistPurged)(replicaID, fw), DotFun[RGANode[E]].empty),
          CContext[C].fromSet(toRemove)
        )
    }

  def clear[E, C: CContext](): DeltaMutator[State[E, C]] = {
    case (_, Causal(_, cc)) =>
      Causal(
        (UIJDLattice[ForcedWrite.State[GOList.State[Dot]]].bottom, DotFun[RGANode[E]].empty),
        cc
      )
  }
}

class RGA[E, C: CContext](crdt: DeltaCRDT[RGACRDT.State[E, C]]) {
  def read(i: Int): Option[E] = crdt.query(RGACRDT.read(i))

  def size: Int = crdt.query(RGACRDT.size)

  def toList: List[E] = crdt.query(RGACRDT.toList)

  def insert(i: Int, e: E): RGA[E, C] = new RGA(crdt.mutate(RGACRDT.insert(i, e)))

  def prepend(e: E): RGA[E, C] = insert(0, e)

  def append(e: E): RGA[E, C] = insert(size, e)

  def insertAll(i: Int, elems: Iterable[E]): RGA[E, C] = new RGA(crdt.mutate(RGACRDT.insertAll(i, elems)))

  def prependAll(elems: Iterable[E]): RGA[E, C] = insertAll(0, elems)

  def appendAll(elems: Iterable[E]): RGA[E, C] = insertAll(size, elems)

  def update(i: Int, e: E): RGA[E, C] = new RGA(crdt.mutate(RGACRDT.update(i, e)))

  def delete(i: Int): RGA[E, C] = new RGA(crdt.mutate(RGACRDT.delete(i)))

  def purgeTombstones(): RGA[E, C] = new RGA(crdt.mutate(RGACRDT.purgeTombstones()))

  def clear(): RGA[E, C] = new RGA(crdt.mutate(RGACRDT.clear()))

  def processReceivedDeltas(): RGA[E, C] = new RGA(crdt.processReceivedDeltas())
}

object RGA {
  type State[E, C] = RGACRDT.State[E, C]
  type Embedded[E] = DotPair[ForcedWrite.State[GOList.State[Dot]], DotFun[RGANode[E]]]

  def apply[E, C: CContext](antiEntropy: AntiEntropy[State[E, C]]): RGA[E, C] =
    new RGA(DeltaCRDT.empty[State[E, C]](antiEntropy))

  implicit def RGAStateCodec[E: JsonValueCodec, C: JsonValueCodec]
      : JsonValueCodec[Causal[((Long, Map[GOListNode[TimedVal[Dot]], Elem[TimedVal[Dot]]]), Map[Dot, RGANode[E]]), C]] =
    JsonCodecMaker.make(CodecMakerConfig.withMapAsArray(true))
}

class RRGA[E, C: CContext](val crdt: RDeltaCRDT[RGACRDT.State[E, C]]) extends CRDTInterface[RGACRDT.State[E, C]] {
  def read(i: Int): Option[E] = crdt.query(RGACRDT.read(i))

  def size: Int = crdt.query(RGACRDT.size)

  def toList: List[E] = crdt.query(RGACRDT.toList)

  def sequence: Long = crdt.query(RGACRDT.sequence)

  def insert(i: Int, e: E): RRGA[E, C] = new RRGA(crdt.mutate(RGACRDT.insert(i, e)))

  def prepend(e: E): RRGA[E, C] = insert(0, e)

  def append(e: E): RRGA[E, C] = insert(size, e)

  def insertAll(i: Int, elems: Iterable[E]): RRGA[E, C] = new RRGA(crdt.mutate(RGACRDT.insertAll(i, elems)))

  def prependAll(elems: Iterable[E]): RRGA[E, C] = insertAll(0, elems)

  def appendAll(elems: Iterable[E]): RRGA[E, C] = insertAll(size, elems)

  def update(i: Int, e: E): RRGA[E, C] = new RRGA(crdt.mutate(RGACRDT.update(i, e)))

  def delete(i: Int): RRGA[E, C] = new RRGA(crdt.mutate(RGACRDT.delete(i)))

  def updateBy(cond: E => Boolean, e: E): RRGA[E, C] = new RRGA(crdt.mutate(RGACRDT.updateBy(cond, e)))

  def deleteBy(cond: E => Boolean): RRGA[E, C] = new RRGA(crdt.mutate(RGACRDT.deleteBy(cond)))

  def purgeTombstones(): RRGA[E, C] = new RRGA(crdt.mutate(RGACRDT.purgeTombstones()))

  def clear(): RRGA[E, C] = new RRGA(crdt.mutate(RGACRDT.clear()))

  def applyDelta(delta: Delta[RGACRDT.State[E, C]]): RRGA[E, C] = {
    val newCRDT = crdt.applyDelta(delta)
    if (newCRDT == crdt) this
    else new RRGA(newCRDT)
  }
}

object RRGA {
  type State[E, C] = RGACRDT.State[E, C]
  type Embedded[E] = DotPair[ForcedWrite.State[GOList.State[Dot]], DotFun[RGANode[E]]]

  def apply[E, C: CContext](replicaID: String): RRGA[E, C] =
    new RRGA(RDeltaCRDT.empty[State[E, C]](replicaID))

  implicit def RGAStateCodec[E: JsonValueCodec, C: JsonValueCodec]
      : JsonValueCodec[Causal[((Long, Map[GOListNode[TimedVal[Dot]], Elem[TimedVal[Dot]]]), Map[Dot, RGANode[E]]), C]] =
    JsonCodecMaker.make(CodecMakerConfig.withMapAsArray(true))
}
