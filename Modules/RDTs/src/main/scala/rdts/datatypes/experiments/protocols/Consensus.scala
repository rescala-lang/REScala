package rdts.datatypes.experiments.protocols

import rdts.base.{Bottom, Lattice, LocalUid, Uid}

// Type class for consensus algorithms
trait Consensus[C[_]] {
  extension [A](c: C[A]) def write(value: A)(using LocalUid): C[A]
  extension [A](c: C[A]) def read: Option[A]
  extension [A](c: C[A]) def members: Set[Uid]
  extension [A](c: C[A]) def upkeep()(using LocalUid): C[A]
  
  def init[A](members: Set[Uid]): C[A]
  def empty[A]: C[A]
  def lattice[A]: Lattice[C[A]]
}

object Consensus {
  given lattice[A, C[_]: Consensus]: Lattice[C[A]] = Consensus[C].lattice
  given bottom[A, C[_]: Consensus]: Bottom[C[A]] with
    override def empty: C[A] = Consensus[C].empty

  def apply[C[_]](using ev: Consensus[C]): Consensus[C] = ev
  def init[A, C[_]](newMembers: Set[Uid])(using Consensus[C]): C[A] =
    val a: Consensus[C] = apply[C]
    a.init(newMembers)
}
