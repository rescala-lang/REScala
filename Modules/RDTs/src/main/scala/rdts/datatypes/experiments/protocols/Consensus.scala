package rdts.datatypes.experiments.protocols

import rdts.base.{LocalUid, Uid}

// Type class for consensus algorithms
trait Consensus[C[_]] {
  extension [A](c: C[A]) def write(value: A)(using LocalUid): C[A]
  extension [A](c: C[A]) def read: Option[A]
  extension [A](c: C[A]) def members: Set[Uid]
  extension [A](c: C[A]) def reset(newMembers: Set[Uid]): C[A]
  extension [A](c: C[A]) def upkeep()(using LocalUid): C[A]
}
