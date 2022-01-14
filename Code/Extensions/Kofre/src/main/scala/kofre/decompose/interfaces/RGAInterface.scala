package kofre.decompose.interfaces

import kofre.decompose.CRDTInterface.{DeltaMutator, DeltaQuery}
import kofre.decompose.DotStore.{DotFun, DotLess, DotPair}
import kofre.decompose._
import kofre.decompose.interfaces.ForcedWriteInterface.ForcedWriteAsUIJDLattice
import kofre.decompose.interfaces.GListInterface.GListAsUIJDLattice

object RGAInterface {
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
      override def decompose(state: RGANode[A]): Iterable[RGANode[A]] = List(state)

      override def bottom: RGANode[A] = throw new UnsupportedOperationException("RGANode does not have a bottom value")

      /** By assumption: associative, commutative, idempotent. */
      override def merge(left: RGANode[A], right: RGANode[A]): RGANode[A] = (left, right) match {
        case (Alive(lv), Alive(rv)) => Alive(UIJDLattice[TimedVal[A]].merge(lv, rv))
        case _                      => Dead()
      }
    }
  }

  type State[E, C] = Causal[DotPair[ForcedWriteInterface.State[GListInterface.State[Dot]], DotFun[RGANode[E]]], C]

  trait RGACompanion {
    type State[E, C] = RGAInterface.State[E, C]
    type Embedded[E] = DotPair[ForcedWriteInterface.State[GListInterface.State[Dot]], DotFun[RGANode[E]]]

    implicit val ForcedWriteAsUIJDLattice: UIJDLattice[ForcedWriteInterface.State[GListInterface.State[Dot]]] =
      ForcedWriteInterface.ForcedWriteAsUIJDLattice[GListInterface.State[Dot]]
  }

  private class DeltaStateFactory[E, C: CContext] {
    val bottom: State[E, C] = UIJDLattice[State[E, C]].bottom

    def make(
        fw: ForcedWriteInterface.State[GListInterface.State[Dot]] = bottom.dotStore._1,
        df: DotFun[RGANode[E]] = bottom.dotStore._2,
        cc: C = bottom.cc
    ): State[E, C] = Causal((fw, df), cc)
  }

  private def deltaState[E, C: CContext]: DeltaStateFactory[E, C] = new DeltaStateFactory[E, C]

  def read[E, C: CContext](i: Int): DeltaQuery[State[E, C], Option[E]] = {
    case Causal((fw, df), _) =>
      GListInterface.toLazyList(fw.value).map(df).collect {
        case Alive(tv) => tv.value
      }.lift(i)
  }

  def size[E, C: CContext]: DeltaQuery[State[E, C], Int] = {
    case Causal((_, df), _) =>
      df.values.count {
        case Dead()   => false
        case Alive(_) => true
      }
  }

  def toList[E, C: CContext]: DeltaQuery[State[E, C], List[E]] = {
    case Causal((fw, df), _) =>
      GListInterface.toList(fw.value).map(df).collect {
        case Alive(tv) => tv.value
      }
  }

  def sequence[E, C: CContext]: DeltaQuery[State[E, C], Long] = {
    case Causal((fw, _), _) => fw.counter
  }

  private def findInsertIndex[E, C: CContext](state: State[E, C], n: Int): Option[Int] = state match {
    case Causal((fw, df), _) =>
      GListInterface.toLazyList(fw.value).zip(LazyList.from(1)).filter {
        case (dot, _) => df(dot) match {
            case Alive(_) => true
            case Dead()   => false
          }
      }.map(_._2).prepended(0).lift(n)
  }

  def insert[E, C: CContext](i: Int, e: E): DeltaMutator[State[E, C]] = {
    case (replicaID, state @ Causal((fw, _), cc)) =>
      val nextDot = CContext[C].nextDot(cc, replicaID)

      findInsertIndex(state, i) match {
        case None => deltaState[E, C].bottom
        case Some(glistInsertIndex) =>
          val m          = GListInterface.insert(glistInsertIndex, nextDot)
          val glistDelta = ForcedWriteInterface.mutate(m)(replicaID, fw)
          val dfDelta    = DotFun[RGANode[E]].empty + (nextDot -> Alive(TimedVal(e, replicaID)))

          deltaState[E, C].make(
            fw = glistDelta,
            df = dfDelta,
            cc = CContext[C].one(nextDot)
          )
      }
  }

  def insertAll[E, C: CContext](i: Int, elems: Iterable[E]): DeltaMutator[State[E, C]] = {
    case (replicaID, state @ Causal((fw, _), cc)) =>
      val nextDot = CContext[C].nextDot(cc, replicaID)

      val nextDots = List.iterate(nextDot, elems.size) {
        case Dot(c, r) => Dot(c + 1, r)
      }

      findInsertIndex(state, i) match {
        case None => deltaState[E, C].bottom
        case Some(glistInsertIndex) =>
          val m          = GListInterface.insertAll(glistInsertIndex, nextDots)
          val glistDelta = ForcedWriteInterface.mutate(m)(replicaID, fw)
          val dfDelta    = DotFun[RGANode[E]].empty ++ (nextDots zip elems.map(e => Alive(TimedVal(e, replicaID))))

          deltaState[E, C].make(
            fw = glistDelta,
            df = dfDelta,
            cc = CContext[C].fromSet(nextDots.toSet)
          )
      }
  }

  private def updateRGANode[E, C: CContext](state: State[E, C], i: Int, newNode: RGANode[E]): State[E, C] =
    state match {
      case Causal((fw, df), _) =>
        GListInterface.toLazyList(fw.value).filter { dot =>
          df(dot) match {
            case Alive(_) => true
            case Dead()   => false
          }
        }.lift(i) match {
          case None => deltaState[E, C].bottom
          case Some(d) =>
            deltaState[E, C].make(df = DotFun[RGANode[E]].empty + (d -> newNode))
        }
    }

  def update[E, C: CContext](i: Int, e: E): DeltaMutator[State[E, C]] =
    (replicaID, state) => updateRGANode(state, i, Alive(TimedVal(e, replicaID)))

  def delete[E, C: CContext](i: Int): DeltaMutator[State[E, C]] = (_, state) => updateRGANode(state, i, Dead[E]())

  private def updateRGANodeBy[E, C: CContext](
      state: State[E, C],
      cond: E => Boolean,
      newNode: RGANode[E]
  ): State[E, C] =
    state match {
      case Causal((_, df), _) =>
        val toUpdate = df.toList.collect {
          case (d, Alive(tv)) if cond(tv.value) => d
        }

        val dfDelta = DotFun[RGANode[E]].empty ++ toUpdate.map(_ -> newNode)

        deltaState[E, C].make(df = dfDelta)
    }

  def updateBy[E, C: CContext](cond: E => Boolean, e: E): DeltaMutator[State[E, C]] =
    (replicaID, state) => updateRGANodeBy(state, cond, Alive(TimedVal(e, replicaID)))

  def deleteBy[E, C: CContext](cond: E => Boolean): DeltaMutator[State[E, C]] =
    (_, state) => updateRGANodeBy(state, cond, Dead[E]())

  def purgeTombstones[E, C: CContext](): DeltaMutator[State[E, C]] = (replicaID, state) =>
    state match {
      case Causal((fw, df), _) =>
        val toRemove = df.collect {
          case (dot, Dead()) => dot
        }.toSet

        val golistPurged = GListInterface.without(fw.value, toRemove)

        deltaState[E, C].make(
          fw = ForcedWriteInterface.forcedWrite(golistPurged)(replicaID, fw),
          cc = CContext[C].fromSet(toRemove)
        )
    }

  def clear[E, C: CContext](): DeltaMutator[State[E, C]] = {
    case (_, Causal(_, cc)) =>
      deltaState[E, C].make(
        cc = cc
      )
  }
}

/** An RGA (Replicated Growable Array) is a Delta CRDT modeling a list.
  *
  * When two values are concurrently inserted at an index i, the value of the insert operation with the later timestamp
  * will be at index i while the earlier inserted value will be pushed to index i+1. When an element is subject to two
  * concurrent updates, the later update overwrites the earlier update. If an element is concurrently updated and deleted,
  * the element will simply be deleted, ignoring the update.
  *
  * Note that RGAs are implemented as linked lists, thus the time needed to execute operations toward the end of the list
  * will scale linearly with the size of the list.
  *
  * To correctly handle concurrent remote inserts next to elements that were deleted locally, the RGA implementation internally
  * keeps deleted elements as hidden tombstones in the list. Since many tombstones will slow down the operations on this
  * data structure, purgeTombstones should be executed periodically to remove tombstones from the list. Note however that
  * this invalidates any concurrent insert operations. Ideally, purgeTombstones should only be called in downtime periods
  * and only by privileged replicas.
  *
  * This implementation was modeled after the RGA proposed by Roh et al. in "Replicated abstract data types: Building blocks
  * for collaborative applications", see [[https://www.sciencedirect.com/science/article/pii/S0743731510002716?casa_token=lQaLin7aEvcAAAAA:Esc3h3WvkFHUcvhalTPPvV5HbJge91D4-2jyKiSlz8GBDjx31l4xvfH8DIstmQ973PVi46ckXHg here]]
  */
abstract class RGAInterface[E, C: CContext, Wrapper] extends CRDTInterface[RGAInterface.State[E, C], Wrapper] {
  def read(i: Int): Option[E] = query(RGAInterface.read(i))

  def size: Int = query(RGAInterface.size)

  def toList: List[E] = query(RGAInterface.toList)

  def sequence: Long = query(RGAInterface.sequence)

  def insert(i: Int, e: E): Wrapper = mutate(RGAInterface.insert(i, e))

  def prepend(e: E): Wrapper = insert(0, e)

  def append(e: E): Wrapper = insert(size, e)

  def insertAll(i: Int, elems: Iterable[E]): Wrapper = mutate(RGAInterface.insertAll(i, elems))

  def prependAll(elems: Iterable[E]): Wrapper = insertAll(0, elems)

  def appendAll(elems: Iterable[E]): Wrapper = insertAll(size, elems)

  def update(i: Int, e: E): Wrapper = mutate(RGAInterface.update(i, e))

  def delete(i: Int): Wrapper = mutate(RGAInterface.delete(i))

  def updateBy(cond: E => Boolean, e: E): Wrapper = mutate(RGAInterface.updateBy(cond, e))

  def deleteBy(cond: E => Boolean): Wrapper = mutate(RGAInterface.deleteBy(cond))

  def purgeTombstones(): Wrapper = mutate(RGAInterface.purgeTombstones())

  def clear(): Wrapper = mutate(RGAInterface.clear())
}
