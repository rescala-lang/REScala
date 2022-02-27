package kofre.causality.impl

import kofre.Defs.{Id, Time}
import kofre.Lattice
import kofre.causality.impl.IntTree
import kofre.causality.Dot

case class TreeCausalContext(internal: Map[Id, IntTree.Tree]) {

  def clockOf(replicaId: Id): Option[Dot] = max(replicaId)

  def add(replicaId: Id, time: Time): TreeCausalContext =
    TreeCausalContext(internal.updated(
      replicaId,
      IntTree.insert(internal.getOrElse(replicaId, IntTree.empty), time)
    ))
  def nextTime(replicaId: Id): Time = {
    val range = internal.getOrElse(replicaId, IntTree.empty)
    IntTree.nextValue(range, 0)
  }
  def nextDot(replicaId: Id): Dot = Dot(replicaId, nextTime(replicaId))
  def diff(extern: TreeCausalContext): TreeCausalContext =
    TreeCausalContext {
      internal.map {
        case (id, range) =>
          val filtered = extern.internal.get(id).map { erange =>
            val keep = IntTree.toSeq(range).filterNot(IntTree.contains(erange, _))
            IntTree.fromIterator(keep.iterator)
          }
          id -> filtered.getOrElse(range)
      }
    }
  def intersect(other: TreeCausalContext): TreeCausalContext =
    TreeCausalContext {
      internal.iterator.filter { case (id, _) => other.internal.contains(id) }.map {
        case (id, range) =>
          val otherRange = other.internal(id)
          val res        = IntTree.fromIterator(IntTree.iterator(range).filter(IntTree.contains(otherRange, _)))
          id -> res
      }.toMap
    }

  def union(other: TreeCausalContext): TreeCausalContext = TreeCausalContext.contextLattice.merge(this, other)

  def contains(d: Dot): Boolean = internal.get(d.replicaId).exists(IntTree.contains(_, d.time))

  def toSet: Set[Dot] =
    internal.flatMap((key, tree) => IntTree.iterator(tree).map(time => Dot(key, time))).toSet

  def max(replicaID: String): Option[Dot] =
    internal.get(replicaID).map(tree => Dot(replicaID, IntTree.nextValue(tree, Long.MinValue) - 1))
      .filterNot(_.time == Long.MinValue)
  def decompose(exclude: Dot => Boolean): Iterable[TreeCausalContext] =
    internal.flatMap { (id, tree) =>
      IntTree.iterator(tree).map(time => Dot(id, time)).filterNot(exclude).map(TreeCausalContext.one)
    }
  def forall(cond: Dot => Boolean): Boolean = internal.forall { (id, tree) =>
    IntTree.iterator(tree).forall(time => cond(Dot(id, time)))
  }
}

object TreeCausalContext {
  def single(replicaId: Id, time: Long): TreeCausalContext =
    TreeCausalContext(Map((replicaId, IntTree.insert(IntTree.empty, time))))
  val empty: TreeCausalContext         = TreeCausalContext(Map.empty)
  def one(dot: Dot): TreeCausalContext = TreeCausalContext.empty.add(dot.replicaId, dot.time)

  implicit val contextLattice: Lattice[TreeCausalContext] = new Lattice[TreeCausalContext] {
    override def merge(left: TreeCausalContext, right: TreeCausalContext): TreeCausalContext = {
      TreeCausalContext(Lattice.merge(left.internal, right.internal))
    }
  }

  def fromSet(dots: Set[Dot]): TreeCausalContext = TreeCausalContext(dots.groupBy(_.replicaId).map {
    (key, times) =>
      key -> IntTree.fromIterator(times.iterator.map(_.time))
  })

}
