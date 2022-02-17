package kofre.causality

import kofre.IdUtil.Id
import kofre.Lattice
import kofre.causality.CausalContext
import kofre.causality.impl.IntTree

case class CausalContext(internal: Map[Id, IntTree.Tree]) {

  def clockOf(replicaId: Id): Option[Dot] = CContext.intTreeCC.max(this, replicaId)

  def add(replicaId: Id, time: Long): CausalContext =
    CausalContext(internal.updated(
      replicaId,
      IntTree.insert(internal.getOrElse(replicaId, IntTree.empty), time)
    ))
  def nextTime(replicaId: Id): Long = {
    val range = internal.getOrElse(replicaId, IntTree.empty)
    IntTree.nextValue(range, 0)
  }
  def diff(extern: CausalContext): CausalContext =
    CausalContext {
      internal.map {
        case (id, range) =>
          val filtered = extern.internal.get(id).map { erange =>
            val keep = IntTree.toSeq(range).filterNot(IntTree.contains(erange, _))
            IntTree.fromIterator(keep.iterator)
          }
          id -> filtered.getOrElse(range)
      }
    }
  def intersect(other: CausalContext): CausalContext =
    CausalContext {
      internal.iterator.filter { case (id, _) => other.internal.contains(id) }.map {
        case (id, range) =>
          val otherRange = other.internal(id)
          val res        = IntTree.fromIterator(IntTree.iterator(range).filter(IntTree.contains(otherRange, _)))
          id -> res
      }.toMap
    }

  def union(other: CausalContext): CausalContext = CausalContext.contextLattice.merge(this, other)

  def contains(d: Dot): Boolean = internal.get(d.replicaID).exists(IntTree.contains(_, d.time))

  def toSet: Set[Dot] =
    internal.flatMap((key, tree) => IntTree.iterator(tree).map(time => Dot(key, time))).toSet

  def max(replicaID: String): Option[Dot] =
    internal.get(replicaID).map(tree => Dot(replicaID, IntTree.nextValue(tree, Long.MinValue) - 1))
      .filterNot(_.time == Long.MinValue)
  def decompose(exclude: Dot => Boolean): Iterable[CausalContext] =
    internal.flatMap { (id, tree) =>
      IntTree.iterator(tree).map(time => Dot(id, time)).filterNot(exclude).map(CausalContext.one)
    }
  def forall(cond: Dot => Boolean): Boolean = internal.forall { (id, tree) =>
    IntTree.iterator(tree).forall(time => cond(Dot(id, time)))
  }
}

object CausalContext {
  def single(replicaId: Id, time: Long): CausalContext =
    CausalContext(Map((replicaId, IntTree.insert(IntTree.empty, time))))
  val empty: CausalContext         = CausalContext(Map.empty)
  def one(dot: Dot): CausalContext = CausalContext.empty.add(dot.replicaId, dot.time)

  implicit val contextLattice: Lattice[CausalContext] = new Lattice[CausalContext] {
    override def merge(left: CausalContext, right: CausalContext): CausalContext = {
      CausalContext(Lattice.merge(left.internal, right.internal))
    }
  }

  def fromSet(dots: Set[Dot]): CausalContext = CausalContext(dots.groupBy(_.replicaId).map {
    (key, times) =>
      key -> IntTree.fromIterator(times.iterator.map(_.time))
  })

}
