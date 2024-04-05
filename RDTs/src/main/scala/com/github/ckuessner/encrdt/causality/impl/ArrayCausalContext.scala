package com.github.ckuessner.encrdt.causality.impl

import com.github.ckuessner.encrdt.causality
import com.github.ckuessner.encrdt.causality.Dot
import com.github.ckuessner.encrdt.causality.impl.Defs.{Id, Time}
import com.github.ckuessner.encrdt.lattices.GrowOnlyMapLattice.gMapLattice
import com.github.ckuessner.encrdt.lattices.SemiLattice

object Defs {
  type Id   = String
  type Time = Long
}

case class ArrayCausalContext(internal: Map[Id, ArrayRanges]) {

  def rangeAt(replicaId: Id): ArrayRanges = internal.getOrElse(replicaId, ArrayRanges.empty)

  def clockOf(replicaId: Id): Option[Dot] = max(replicaId)

  def add(replicaId: Id, time: Time): ArrayCausalContext =
    ArrayCausalContext(
      internal.updated(
        replicaId,
        rangeAt(replicaId).add(time)
      )
    )

  def nextTime(replicaId: Id): Time = rangeAt(replicaId).next.getOrElse(0)

  def nextDot(replicaId: Id): Dot = Dot(nextTime(replicaId), replicaId)

  def diff(extern: ArrayCausalContext): ArrayCausalContext =
    ArrayCausalContext(
      internal.map { case (id, range) =>
        val filtered = extern.internal.get(id).map { erange =>
          val keep = range.iterator.filterNot(erange.contains)
          ArrayRanges.from(keep)
        }
        id -> filtered.getOrElse(range)
      }
    )

  def intersect(other: ArrayCausalContext): ArrayCausalContext =
    ArrayCausalContext {
      internal.flatMap { case (id, ranges) =>
        other.internal.get(id) match {
          case Some(otherRanges) =>
            val intersection = ranges.intersect(otherRanges)
            if (intersection.isEmpty) None
            else Some(id -> intersection)
          case None => None
        }
      }
    }

  def union(other: ArrayCausalContext): ArrayCausalContext = ArrayCausalContext.contextLattice.merged(this, other)

  def subtract(other: ArrayCausalContext): ArrayCausalContext = {
    ArrayCausalContext(
      internal
        .map { case left @ (id, leftRanges) =>
          other.internal.get(id) match {
            case Some(rightRanges) => id -> leftRanges.subtract(rightRanges)
            case None              => left
          }
        }
        .filterNot(_._2.isEmpty)
    )
  }

  def contains(d: Dot): Boolean = internal.get(d.replicaId).exists(_.contains(d.time))

  def toSet: Set[Dot] =
    internal.flatMap { case (key, tree) => tree.iterator.map(time => causality.Dot(time, key)) }.toSet

  def max(replicaID: String): Option[Dot] =
    internal.get(replicaID).flatMap(_.next.map(c => Dot(c - 1, replicaID)))

  def decompose(exclude: Dot => Boolean): Iterable[ArrayCausalContext] =
    internal.flatMap { case (id, tree) =>
      tree.iterator.map(time => causality.Dot(time, id)).filterNot(exclude).map(ArrayCausalContext.one)
    }
  def forall(cond: Dot => Boolean): Boolean = internal.forall { case (id, tree) =>
    tree.iterator.forall(time => cond(causality.Dot(time, id)))
  }

  def <=(other: ArrayCausalContext): Boolean = internal.forall { case (id, leftRange) =>
    leftRange <= other.rangeAt(id)
  }
}

object ArrayCausalContext {
  def single(replicaId: Id, time: Long): ArrayCausalContext = empty.add(replicaId, time)

  def single(dot: Dot): ArrayCausalContext = empty.add(dot.replicaId, dot.time)

  val empty: ArrayCausalContext = ArrayCausalContext(Map.empty)

  def one(dot: Dot): ArrayCausalContext = empty.add(dot.replicaId, dot.time)

  given contextLattice: SemiLattice[ArrayCausalContext] =
    (left: ArrayCausalContext, right: ArrayCausalContext) => {
      ArrayCausalContext(SemiLattice.merged(left.internal, right.internal))
    }

  def fromSet(dots: Set[Dot]): ArrayCausalContext = ArrayCausalContext(
    dots.groupBy(_.replicaId).map { case (key, times) =>
      key -> ArrayRanges.from(times.iterator.map(_.time))
    }
  )

}
