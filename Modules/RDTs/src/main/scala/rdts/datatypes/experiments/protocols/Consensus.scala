package rdts.datatypes.experiments.protocols

import rdts.base.{LocalUid, Uid}

// Type class for consensus algorithms
trait Consensus[C[_]] {
  extension [A](c: C[A]) def write(value: A)(using LocalUid): C[A]
  extension [A](c: C[A]) def read: Option[A]
  extension [A](c: C[A]) def members: Set[Uid]
  def init[A](members: Set[Uid]): C[A]
  def reset[A](newMembers: Set[Uid]): C[A] = init(newMembers)
  extension [A](c: C[A]) def upkeep()(using LocalUid): C[A]
}

object Consensus {
  given syntax: {} with
    extension [A, C[_]: Consensus](c: C[A])
      def reset(newMembers: Set[Uid]): C[A] = Consensus[C].reset(newMembers)

  def apply[C[_]](using ev: Consensus[C]): Consensus[C] = ev
  def init[A, C[_]](newMembers: Set[Uid])(using Consensus[C]): C[A] =
    val a: Consensus[C] = apply[C]
    a.init(newMembers)
}
