package kofre.causality

import kofre.IdUtil.Id
import kofre.Lattice
import kofre.causality.impl.IntTree
import kofre.causality.CausalContext

case class CausalContext(internal: Map[Id, IntTree.Tree]) {

  def clockOf(replicaId: Id) = CContext.intTreeCC.max(this, replicaId)

  def contains(dot: Dot) = CContext.intTreeCC.contains(this, dot)

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

  def merged(other: CausalContext): CausalContext = CausalContext.contextLattice.merge(this, other)
}

object CausalContext {
  def single(replicaId: Id, time: Long): CausalContext = CausalContext(Map((replicaId, IntTree.insert(IntTree.empty, time))))
  val empty: CausalContext = CausalContext(Map.empty)

  implicit val contextLattice: Lattice[CausalContext] = new Lattice[CausalContext] {
    override def merge(left: CausalContext, right: CausalContext): CausalContext = {
      CausalContext(Lattice.merge(left.internal, right.internal))
    }
  }

  def fromSet(dots: Set[Dot]): CausalContext = CContext.intTreeCC.fromSet(dots)
}
