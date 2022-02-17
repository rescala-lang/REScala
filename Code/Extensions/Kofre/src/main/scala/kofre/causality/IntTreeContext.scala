package kofre.causality

import kofre.IdUtil.Id
import kofre.Lattice
import kofre.causality.impl.IntTree
import kofre.causality.IntTreeContext

case class IntTreeContext(internal: Map[Id, IntTree.Tree]) {

  def add(replicaId: Id, time: Int): IntTreeContext =
    IntTreeContext(internal.updated(
      replicaId,
      IntTree.insert(internal.getOrElse(replicaId, IntTree.empty), time)
    ))
  def nextTime(replicaId: Id): Int = {
    val range = internal.getOrElse(replicaId, IntTree.empty)
    IntTree.nextValue(range, 0)
  }
  def diff(extern: IntTreeContext): IntTreeContext =
    IntTreeContext {
      internal.map {
        case (id, range) =>
          val filtered = extern.internal.get(id).map { erange =>
            val keep = IntTree.toSeq(range).filterNot(IntTree.contains(erange, _))
            IntTree.fromIterator(keep.iterator)
          }
          id -> filtered.getOrElse(range)
      }
    }
  def intersect(other: IntTreeContext): IntTreeContext =
    IntTreeContext {
      internal.iterator.filter { case (id, _) => other.internal.contains(id) }.map {
        case (id, range) =>
          val otherRange = other.internal(id)
          val res        = IntTree.fromIterator(IntTree.iterator(range).filter(IntTree.contains(otherRange, _)))
          id -> res
      }.toMap
    }
}

object IntTreeContext {
  def single(replicaId: Id, time: Int): IntTreeContext = IntTreeContext(Map((replicaId, IntTree.insert(IntTree.empty, time))))
  val empty: IntTreeContext = IntTreeContext(Map.empty)

  implicit val contextLattice: Lattice[IntTreeContext] = new Lattice[IntTreeContext] {
    override def merge(left: IntTreeContext, right: IntTreeContext): IntTreeContext = {
      IntTreeContext(Lattice.merge(left.internal, right.internal))
    }
  }
}
