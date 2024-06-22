package rdts.datatypes.experiments.protocols

import rdts.base.{Bottom, Lattice, Orderings, Uid}
import rdts.datatypes.contextual.ReplicatedSet
import rdts.dotted.{Dotted, HasDots}
import rdts.syntax.LocalUid
import rdts.syntax.LocalUid.replicaId
import rdts.time.Dots

case class Ownership(epoch: Long, owner: Uid)

object Ownership {
  given Lattice[Ownership] = Lattice.fromOrdering(using Orderings.lexicographic)

  given bottom: Bottom[Ownership] = Bottom.provide(Ownership(Long.MinValue, Uid.zero))

  def unchanged: Ownership = bottom.empty
}

case class Token(os: Ownership, wants: ReplicatedSet[Uid]) {

  def isOwner(using LocalUid): Boolean = replicaId == os.owner

  def request(using LocalUid, Dots): Dotted[Token] =
    wants.add(replicaId).map(w => Token(Ownership.unchanged, w))

  def release(using LocalUid, Dots): Dotted[Token] =
    wants.remove(replicaId).map(w => Token(Ownership.unchanged, w))

  def upkeep(using LocalUid): Token =
    if !isOwner then Token.unchanged
    else
      selectFrom(wants) match
        case None => Token.unchanged
        case Some(nextOwner) =>
          Token(Ownership(os.epoch + 1, nextOwner), ReplicatedSet.empty)

  def selectFrom(wants: ReplicatedSet[Uid])(using LocalUid) =
    // We find the “largest” ID that wants the token.
    // This is incredibly “unfair” but does prevent deadlocks in case someone needs multiple tokens.
    wants.elements.maxOption.filter(id => id != replicaId)

}

object Token {
  val unchanged: Token = Token(Ownership.unchanged, ReplicatedSet.empty)
  given Lattice[Token] = Lattice.derived
}

case class ExampleTokens(
    calendarAinteractionA: Token,
    calendarBinteractionA: Token
)

case class Exclusive[T: Lattice: Bottom](token: Token, value: T) {
  def transform(f: T => T)(using LocalUid) =
    if token.isOwner then f(value) else Bottom.empty
}

/** totally not incredibly inefficient */
case class Causal[T: Lattice: HasDots: Bottom](deltas: Set[Dotted[T]]) {
  def value: T =
    val causalPrefix = deltas.map(_.context).reduceOption(_ union _).map(_.causalPrefix).getOrElse(Dots.empty)
    deltas.filter(delta => delta.context <= causalPrefix).reduceOption(Dotted.lattice.merge).map(_.data).getOrElse(
      Bottom.empty
    )
}
