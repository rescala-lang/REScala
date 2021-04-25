package rescala.extra.lattices.delta.crdt

import com.github.plokhotnyuk.jsoniter_scala.core.JsonValueCodec
import com.github.plokhotnyuk.jsoniter_scala.macros.{CodecMakerConfig, JsonCodecMaker}
import rescala.extra.lattices.delta.DeltaCRDT.{DeltaMutator, DeltaQuery}
import rescala.extra.lattices.delta.DotStore.DotFun
import rescala.extra.lattices.delta._
import rescala.extra.lattices.delta.crdt.GOListCRDT.GOListAsUIJDLattice

import scala.annotation.tailrec

sealed trait RGANode[A]
case class Alive[A](v: TimedVal[A]) extends RGANode[A]
case class Dead[A]() extends RGANode[A]

object RGANode {
  implicit def RGANodeAsUIJDLattice[A]: UIJDLattice[RGANode[A]] = new UIJDLattice[RGANode[A]] {
    override def leq(left: RGANode[A], right: RGANode[A]): Boolean = (left, right) match {
      case (Dead(), _) => false
      case (_, Dead()) => true
      case (Alive(lv), Alive(rv)) => rv.laterThan(lv)
    }

    /**
      * Decomposes a lattice state into its unique irredundant join decomposition of join-irreducable states
      */
    override def decompose(state: RGANode[A]): Set[RGANode[A]] = Set(state)

    override def bottom: RGANode[A] = throw new UnsupportedOperationException("RGANode does not have a bottom value")

    /** By assumption: associative, commutative, idempotent. */
    override def merge(left: RGANode[A], right: RGANode[A]): RGANode[A] = (left, right) match {
      case (Alive(lv), Alive(rv)) => Alive(UIJDLattice[TimedVal[A]].merge(lv, rv))
      case _ => Dead()
    }
  }
}

object RGACRDT {
  type State[E, C] = (GOList.State[Dot], Causal[DotFun[RGANode[E]], C])

  private def readRec[E, C: CContext](state: State[E, C], target: Int, counted: Int, skipped: Int): Option[E] = state match {
    case (golist, Causal(df, _)) =>
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
    case (_, Causal(df, _)) =>
      df.values.count {
        case Dead() => false
        case Alive(_) => true
      }
  }

  def toList[E, C: CContext]: DeltaQuery[State[E, C], List[E]] = {
    case (golist, Causal(df, _)) =>
      GOListCRDT.toList(golist).flatMap(df.get).collect {
        case Alive(tv) => tv.value
      }
  }

  def insertRec[E, C: CContext](replicaID: String, state: State[E, C], target: Int, counted: Int, skipped: Int): Option[GOList.State[Dot]] =
    state match {
      case (golist, Causal(df, cc)) =>
        if (target == counted)
          Some(GOListCRDT.insert(counted + skipped, CContext[C].nextDot(cc, replicaID))(replicaID, golist))
        else {
          GOListCRDT.read(counted + skipped)(golist) flatMap { d =>
            df(d) match {
              case Dead() => insertRec(replicaID, state, target, counted, skipped + 1)
              case Alive(_) => insertRec(replicaID, state, target, counted + 1, skipped)
            }
          }
        }
    }

  def insert[E, C: CContext](i: Int, e: E): DeltaMutator[State[E, C]] = {
    case (replicaID, state@(golist, Causal(_, cc))) =>
      val nextDot = CContext[C].nextDot(cc, replicaID)

      insertRec(replicaID, state, i, 0, 0) match {
        case None => UIJDLattice[State[E, C]].bottom
        case Some(golistDelta) =>
          val dfDelta = DotFun[RGANode[E]].empty + (nextDot -> Alive(TimedVal(e, replicaID, golist.size)))

          (golistDelta, Causal(dfDelta, CContext[C].fromSet(Set(nextDot))))
      }
  }

  @tailrec
  private def updateRGANodeRec[E, C: CContext](state: State[E, C], i: Int, newNode: RGANode[E], counted: Int, skipped: Int): State[E, C] =
    state match {
      case (golist, Causal(df, _)) =>
        GOListCRDT.read(counted + skipped)(golist) match {
          case None => UIJDLattice[State[E, C]].bottom
          case Some(d) =>
            df(d) match {
              case Dead() => updateRGANodeRec(state, i, newNode, counted, skipped + 1)
              case Alive(_) =>
                if (counted == i)
                  (UIJDLattice[GOList.State[Dot]].bottom, Causal(DotFun[RGANode[E]].empty + (d -> newNode), CContext[C].empty))
                else
                  updateRGANodeRec(state, i, newNode, counted + 1, skipped)
            }
        }
    }

  private def updateRGANode[E, C: CContext](state: State[E, C], i: Int, newNode: RGANode[E]): State[E, C] =
    updateRGANodeRec(state, i, newNode, 0, 0)

  def update[E, C: CContext](i: Int, e: E): DeltaMutator[State[E, C]] = (replicaID, state) =>
    // Assume that a replica never updates the element twice in the same time unit
    // Otherwise, one would have to derive some counter value that increases each time an element is updated
    updateRGANode(state, i, Alive(TimedVal(e, replicaID, 0)))

  def delete[E, C: CContext](i: Int): DeltaMutator[State[E, C]] = (_, state) =>
    updateRGANode(state, i, Dead[E]())

  private def updateRGANodeBy[E, C: CContext](state: State[E, C], cond: E => Boolean, newNode: RGANode[E]): State[E, C] =
    state match {
      case (_, Causal(df, _)) =>
        val toUpdate = df.toList.collect {
          case (d, Alive(tv)) if cond(tv.value) => d
        }

        val dfDelta = DotFun[RGANode[E]].empty ++ toUpdate.map(_ -> newNode)

        (UIJDLattice[GOList.State[Dot]].bottom, Causal(dfDelta, CContext[C].empty))
  }

  def updateBy[E, C: CContext](cond: E => Boolean, e: E): DeltaMutator[State[E, C]] = (replicaID, state) =>
    updateRGANodeBy(state, cond, Alive(TimedVal(e, replicaID, 0)))

  def deleteBy[E, C: CContext](cond: E => Boolean): DeltaMutator[State[E, C]] = (_, state) =>
    updateRGANodeBy(state, cond, Dead[E]())
}

class RGA[E, C: CContext](crdt: DeltaCRDT[RGACRDT.State[E, C]]) {
  def read(i: Int): Option[E] = crdt.query(RGACRDT.read(i))

  def size: Int = crdt.query(RGACRDT.size)

  def toList: List[E] = crdt.query(RGACRDT.toList)

  def insert(i: Int, e: E): RGA[E, C] = new RGA(crdt.mutate(RGACRDT.insert(i, e)))

  def update(i: Int, e: E): RGA[E, C] = new RGA(crdt.mutate(RGACRDT.update(i, e)))

  def delete(i: Int): RGA[E, C] = new RGA(crdt.mutate(RGACRDT.delete(i)))

  def processReceivedDeltas(): RGA[E, C] = new RGA(crdt.processReceivedDeltas())
}

object RGA {
  type State[E, C] = RGACRDT.State[E, C]
  // To embed an RGA, the state would need to change to a Causal[DotPair[DotLess[GOList.State[Dot]], DotFun[RGANode[E]]]]]
  // Would be a bit more ugly but maybe worth...?

  def apply[E, C: CContext](antiEntropy: AntiEntropy[State[E, C]]): RGA[E, C] =
    new RGA(DeltaCRDT.empty[State[E, C]](antiEntropy))

  implicit def RGAStateCodec[E: JsonValueCodec, C: JsonValueCodec]: JsonValueCodec[(Map[GOListNode[TimedVal[Dot]], Elem[TimedVal[Dot]]], Causal[Map[Dot, RGANode[E]], C])] =
    JsonCodecMaker.make(CodecMakerConfig.withMapAsArray(true).withTransientDefault(false))
}

class RRGA[E, C: CContext](val crdt: RDeltaCRDT[RGACRDT.State[E, C]]) extends CRDTInterface[RGACRDT.State[E, C]] {
  def read(i: Int): Option[E] = crdt.query(RGACRDT.read(i))

  def size: Int = crdt.query(RGACRDT.size)

  def toList: List[E] = crdt.query(RGACRDT.toList)

  def insert(i: Int, e: E): RRGA[E, C] = new RRGA(crdt.mutate(RGACRDT.insert(i, e)))

  def update(i: Int, e: E): RRGA[E, C] = new RRGA(crdt.mutate(RGACRDT.update(i, e)))

  def delete(i: Int): RRGA[E, C] = new RRGA(crdt.mutate(RGACRDT.delete(i)))

  def updateBy(cond: E => Boolean, e: E): RRGA[E, C] = new RRGA(crdt.mutate(RGACRDT.updateBy(cond, e)))

  def deleteBy(cond: E => Boolean): RRGA[E, C] = new RRGA(crdt.mutate(RGACRDT.deleteBy(cond)))

  def applyDelta(delta: Delta[RGACRDT.State[E, C]]): RRGA[E, C] = {
    val newCRDT = crdt.applyDelta(delta)
    if (newCRDT == crdt) this
    else new RRGA(newCRDT)
  }
}

object RRGA {
  type State[E, C] = RGACRDT.State[E, C]
  // To embed an RGA, the state would need to change to a Causal[DotPair[DotLess[GOList.State[Dot]], DotFun[RGANode[E]]]]]
  // Would be a bit more ugly but maybe worth...?

  def apply[E, C: CContext](replicaID: String): RRGA[E, C] =
    new RRGA(RDeltaCRDT.empty[State[E, C]](replicaID))
}


