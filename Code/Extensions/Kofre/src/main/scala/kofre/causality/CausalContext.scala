package kofre.causality

import kofre.Defs.{Id, Time}
import kofre.Lattice
import kofre.causality.Dot

case class CausalContext(internal: Map[Id, ArrayRanges]) {

  def rangeAt(replicaId: Id): ArrayRanges = internal.getOrElse(replicaId, ArrayRanges.empty)

  def clockOf(replicaId: Id): Option[Dot] = max(replicaId)

  def add(dot: Dot): CausalContext = add(dot.replicaId, dot.time)

  def add(replicaId: Id, time: Time): CausalContext =
    CausalContext(internal.updated(
      replicaId,
      rangeAt(replicaId).add(time)
    ))

  def nextTime(replicaId: Id): Time = rangeAt(replicaId).next.getOrElse(0)

  def nextDot(replicaId: Id): Dot = Dot(replicaId, nextTime(replicaId))

  def diff(extern: CausalContext): CausalContext =
    CausalContext(
      internal.map {
        case (id, range) =>
          val filtered = extern.internal.get(id).map { erange =>
            val keep = range.iterator.filterNot(erange.contains)
            ArrayRanges.from(keep)
          }
          id -> filtered.getOrElse(range)
      }
    )

  def intersect(other: CausalContext): CausalContext =
    CausalContext {
      internal.iterator.filter { case (id, _) => other.internal.contains(id) }.map {
        case (id, range) =>
          val otherRange = other.internal(id)
          val res        = ArrayRanges.from(range.iterator.filter(otherRange.contains))
          id -> res
      }.toMap
    }

  def union(other: CausalContext): CausalContext = CausalContext.contextLattice.merge(this, other)

  def contains(d: Dot): Boolean = internal.get(d.replicaId).exists(_.contains(d.time))

  def toSet: Set[Dot] =
    internal.flatMap((key, tree) => tree.iterator.map(time => Dot(key, time))).toSet

  def max(replicaID: String): Option[Dot] =
    internal.get(replicaID).flatMap(_.next.map(c => Dot(replicaID, c - 1)))

  def decompose(exclude: Dot => Boolean): Iterable[CausalContext] =
    internal.flatMap { (id, tree) =>
      tree.iterator.map(time => Dot(id, time)).filterNot(exclude).map(CausalContext.one)
    }
  def forall(cond: Dot => Boolean): Boolean = internal.forall { (id, tree) =>
    tree.iterator.forall(time => cond(Dot(id, time)))
  }
}

object CausalContext {
  def single(replicaId: Id, time: Long): CausalContext = empty.add(replicaId, time)

  val empty: CausalContext = CausalContext(Map.empty)

  def one(dot: Dot): CausalContext = empty.add(dot.replicaId, dot.time)

  implicit val contextLattice: Lattice[CausalContext] = new Lattice[CausalContext] {
    override def merge(left: CausalContext, right: CausalContext): CausalContext = {
      CausalContext(Lattice.merge(left.internal, right.internal))
    }
  }

  def fromSet(dots: Set[Dot]): CausalContext = CausalContext(dots.groupBy(_.replicaId).map {
    (key, times) =>
      key -> ArrayRanges.from(times.iterator.map(_.time))
  })

}
