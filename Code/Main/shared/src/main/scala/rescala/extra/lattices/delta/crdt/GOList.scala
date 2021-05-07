package rescala.extra.lattices.delta.crdt

import com.github.plokhotnyuk.jsoniter_scala.core.JsonValueCodec
import com.github.plokhotnyuk.jsoniter_scala.macros.{CodecMakerConfig, JsonCodecMaker}
import rescala.extra.lattices.delta.DeltaCRDT.{DeltaMutator, DeltaQuery}
import rescala.extra.lattices.delta.crdt.GOListCRDT.GOListAsUIJDLattice
import rescala.extra.lattices.delta.{AntiEntropy, Delta, DeltaCRDT, RDeltaCRDT, TimedVal, UIJDLattice}

import scala.annotation.tailrec

sealed trait GOListNode[E]
case class Head[E]()         extends GOListNode[E]
case class Elem[E](value: E) extends GOListNode[E]

object GOListCRDT {
  type State[E] = Map[GOListNode[TimedVal[E]], Elem[TimedVal[E]]]

  implicit def GOListAsUIJDLattice[E]: UIJDLattice[State[E]] =
    new UIJDLattice[Map[GOListNode[TimedVal[E]], Elem[TimedVal[E]]]] {
      override def leq(
          left: Map[GOListNode[TimedVal[E]], Elem[TimedVal[E]]],
          right: Map[GOListNode[TimedVal[E]], Elem[TimedVal[E]]]
      ): Boolean =
        left.toSet.subsetOf(right.toSet)

      /** Decomposes a lattice state into its unique irredundant join decomposition of join-irreducible states */
      override def decompose(state: Map[GOListNode[TimedVal[E]], Elem[TimedVal[E]]])
          : Set[Map[GOListNode[TimedVal[E]], Elem[TimedVal[E]]]] =
        state.toSet.map((edge: (GOListNode[TimedVal[E]], Elem[TimedVal[E]])) => Map(edge))

      override def bottom: Map[GOListNode[TimedVal[E]], Elem[TimedVal[E]]] = Map.empty

      @tailrec
      private def insertEdge(
          state: Map[GOListNode[TimedVal[E]], Elem[TimedVal[E]]],
          edge: (GOListNode[TimedVal[E]], Elem[TimedVal[E]])
      ): Map[GOListNode[TimedVal[E]], Elem[TimedVal[E]]] =
        edge match {
          case (l, r @ Elem(e1)) =>
            state.get(l) match {
              case None => state + edge
              case Some(next @ Elem(e2)) =>
                if (e1.laterThan(e2)) state + edge + (r -> next)
                else insertEdge(state, next -> r)
            }
        }

      @tailrec
      private def insertRec(left: State[E], right: State[E], current: GOListNode[TimedVal[E]]): State[E] =
        right.get(current) match {
          case None => left
          case Some(next) =>
            val leftMerged =
              if (left.contains(current) && left.values.toSet.contains(next))
                left
              else
                insertEdge(left, (current, next))

            insertRec(leftMerged, right, next)
        }

      /** By assumption: associative, commutative, idempotent. */
      override def merge(
          left: Map[GOListNode[TimedVal[E]], Elem[TimedVal[E]]],
          right: Map[GOListNode[TimedVal[E]], Elem[TimedVal[E]]]
      ): Map[GOListNode[TimedVal[E]], Elem[TimedVal[E]]] =
        (right.keySet -- right.values).foldLeft(left) { (state, startNode) =>
          insertRec(state, right, startNode)
        }
    }

  @tailrec
  private def findNth[E](state: State[E], current: GOListNode[TimedVal[E]], i: Int): Option[GOListNode[TimedVal[E]]] = {
    if (i == 0) Some(current)
    else state.get(current) match {
      case None       => None
      case Some(elem) => findNth(state, elem, i - 1)
    }
  }

  def read[E](i: Int): DeltaQuery[State[E], Option[E]] = state =>
    findNth(state, Head[TimedVal[E]](), i + 1).flatMap {
      case Head()  => None
      case Elem(e) => Some(e.value)
    }

  @tailrec
  private def toListRec[E](state: State[E], current: GOListNode[TimedVal[E]], acc: List[E]): List[E] =
    state.get(current) match {
      case None                  => acc
      case Some(next @ Elem(tv)) => toListRec(state, next, acc.appended(tv.value))
    }

  def toList[E]: DeltaQuery[State[E], List[E]] = state => toListRec(state, Head[TimedVal[E]](), List.empty[E])

  def size[E]: DeltaQuery[State[E], Int] = state => state.size

  def insert[E](i: Int, e: E): DeltaMutator[State[E]] = (replicaID, state) => {
    findNth(state, Head[TimedVal[E]](), i) match {
      case None        => Map.empty
      case Some(after) => Map(after -> Elem(TimedVal(e, replicaID)))
    }
  }

  @tailrec
  private def withoutRec[E](state: State[E], current: GOListNode[TimedVal[E]], elems: Set[E]): State[E] =
    state.get(current) match {
      case None => state
      case Some(next @ Elem(tv)) if elems.contains(tv.value) =>
        val edgeRemoved = state.get(next) match {
          case Some(nextnext) => state.removed(current).removed(next) + (current -> nextnext)
          case None           => state.removed(current).removed(next)
        }

        withoutRec(edgeRemoved, current, elems)
      case Some(next) => withoutRec(state, next, elems)
    }

  def without[E](state: State[E], elems: Set[E]): State[E] = withoutRec(state, Head[TimedVal[E]](), elems)
}

class GOList[E](crdt: DeltaCRDT[GOListCRDT.State[E]]) {
  def read(i: Int): Option[E] = crdt.query(GOListCRDT.read(i))

  def toList: List[E] = crdt.query(GOListCRDT.toList)

  def size: Int = crdt.query(GOListCRDT.size)

  def insert(i: Int, e: E): GOList[E] = new GOList(crdt.mutate(GOListCRDT.insert(i, e)))

  def processReceivedDeltas(): GOList[E] = new GOList(crdt.processReceivedDeltas())
}

object GOList {
  type State[E] = GOListCRDT.State[E]

  def apply[E](antiEntropy: AntiEntropy[State[E]]): GOList[E] =
    new GOList(DeltaCRDT.empty[State[E]](antiEntropy))

  implicit def GOListStateCodec[E: JsonValueCodec]: JsonValueCodec[Map[GOListNode[TimedVal[E]], Elem[TimedVal[E]]]] =
    JsonCodecMaker.make(CodecMakerConfig.withMapAsArray(true))
}

class RGOList[E](val crdt: RDeltaCRDT[GOListCRDT.State[E]]) extends CRDTInterface[GOListCRDT.State[E]] {
  def read(i: Int): Option[E] = crdt.query(GOListCRDT.read(i))

  def toList: List[E] = crdt.query(GOListCRDT.toList)

  def size: Int = crdt.query(GOListCRDT.size)

  def insert(i: Int, e: E): RGOList[E] = new RGOList(crdt.mutate(GOListCRDT.insert(i, e)))

  def applyDelta(delta: Delta[GOListCRDT.State[E]]): RGOList[E] = {
    val newCRDT = crdt.applyDelta(delta)

    if (newCRDT == crdt) this
    else new RGOList(newCRDT)
  }
}

object RGOList {
  type State[E] = GOListCRDT.State[E]

  def apply[E](replicaID: String): RGOList[E] =
    new RGOList(RDeltaCRDT.empty[State[E]](replicaID))
}
