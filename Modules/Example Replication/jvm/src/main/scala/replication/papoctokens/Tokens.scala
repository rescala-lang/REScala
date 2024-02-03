package replication.papoctokens

import kofre.base.{Bottom, Lattice, Orderings, Uid}
import kofre.datatypes.contextual.ReplicatedSet
import kofre.datatypes.contextual.ReplicatedSet.syntax
import kofre.datatypes.experiments.RaftState
import kofre.dotted.Dotted
import kofre.syntax.{DeltaBuffer, OpsSyntaxHelper, ReplicaId}

import scala.util.Random

case class Ownership(epoche: Long, owner: Uid)

object Ownership {
  given Lattice[Ownership] = Lattice.fromOrdering(Orderings.lexicographic)

  given bottom: Bottom[Ownership] = Bottom.provide(Ownership(Long.MinValue, Uid.zero))

  def unchanged: Ownership = bottom.empty
}

case class Token(os: Ownership, wants: ReplicatedSet[Uid])
object Token {

  val unchanged: Token = Token(Ownership.unchanged, ReplicatedSet.empty)

  given Lattice[Token] = Lattice.derived

  extension [C, E](container: C)
    def tokens: syntax[C] = syntax(container)

  implicit class syntax[C](container: C) extends OpsSyntaxHelper[C, Token](container) {

    def updateWant(f: Dotted[ReplicatedSet[Uid]] => Dotted[ReplicatedSet[Uid]]): CausalMutate = mutate:
      val addWant: Dotted[ReplicatedSet[Uid]] = f(current.wants.inheritContext)

      addWant.map: aw =>
        Token(Ownership.unchanged, aw)

    def request(using ReplicaId): CausalMutate = updateWant(_.add(replicaId))

    def release(using ReplicaId): CausalMutate = updateWant(_.remove(replicaId))

    def grant(using ReplicaId): Mutate = mutate:
      if !isOwner then unchanged
      else
        selectNewOwner match
          case None => unchanged
          case Some(nextOwner) =>
            Token(Ownership(current.os.epoche + 1, nextOwner), ReplicatedSet.empty)

    def selectNewOwner(using ReplicaId, PermQuery): Option[Uid] =
      // We find the “largest” ID that wants the token.
      // This is incredibly “unfair” but does prevent deadlocks in case someone needs multiple tokens.
      current.wants.elements.maxOption.filter(id => id != replicaId)

    def isOwner(using ReplicaId, PermQuery): Boolean = replicaId == current.os.owner
  }
}

case class ExampleTokens(
    calendarAinteractionA: Token,
    calendarBinteractionA: Token
)
