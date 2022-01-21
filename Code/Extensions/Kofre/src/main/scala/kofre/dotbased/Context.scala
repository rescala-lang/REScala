package kofre.dotbased

import kofre.IdUtil.Id
import kofre.Lattice

case class Context(internal: Map[Id, IntTree.Tree]) {

  def add(replicaId: Id, time: Int): Context =
    Context(internal.updated(
      replicaId,
      IntTree.insert(internal.getOrElse(replicaId, IntTree.empty), time)
    ))
  def nextTime(replicaId: Id): Int = {
    val range = internal.getOrElse(replicaId, IntTree.empty)
    IntTree.nextValue(range, 0)
  }
  def diff(extern: Context): Context =
    Context {
      internal.map {
        case (id, range) =>
          val filtered = extern.internal.get(id).map { erange =>
            val keep = IntTree.toSeq(range).filterNot(IntTree.contains(erange, _))
            IntTree.fromIterator(keep.iterator)
          }
          id -> filtered.getOrElse(range)
      }
    }
  def intersect(other: Context): Context =
    Context {
      internal.iterator.filter { case (id, _) => other.internal.contains(id) }.map {
        case (id, range) =>
          val otherRange = other.internal(id)
          val res        = IntTree.fromIterator(IntTree.iterator(range).filter(IntTree.contains(otherRange, _)))
          id -> res
      }.toMap
    }
}

object Context {
  def single(replicaId: Id, time: Int): Context = Context(Map((replicaId, IntTree.insert(IntTree.empty, time))))
  val empty: Context                            = Context(Map.empty)

  implicit val contextLattice: Lattice[Context] = new Lattice[Context] {
    override def merge(left: Context, right: Context): Context = {
      Context(Lattice.merge(left.internal, right.internal))
    }
  }
}
