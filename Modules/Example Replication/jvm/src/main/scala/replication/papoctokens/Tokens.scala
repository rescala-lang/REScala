package replication.papoctokens

import kofre.base.{Bottom, Lattice, Orderings, Uid}
import kofre.datatypes.Epoche
import kofre.datatypes.contextual.ReplicatedSet
import kofre.datatypes.contextual.ReplicatedSet.syntax
import kofre.datatypes.experiments.RaftState
import kofre.dotted.Dotted
import kofre.syntax.ReplicaId.replicaId
import kofre.syntax.{DeltaBuffer, OpsSyntaxHelper, ReplicaId}
import kofre.time.Dots

import scala.util.Random

case class Ownership(epoche: Long, owner: Uid)

object Ownership {
  given Lattice[Ownership] = Lattice.fromOrdering(Orderings.lexicographic)

  given bottom: Bottom[Ownership] = Bottom.provide(Ownership(Long.MinValue, Uid.zero))

  def unchanged: Ownership = bottom.empty
}

case class Token(os: Ownership, wants: ReplicatedSet[Uid]) {
  def updateWants(dottedWant: Dotted[ReplicatedSet[Uid]]): Dotted[Token] =
    dottedWant.map: wants =>
      Token(Ownership.unchanged, wants)

  def request(using ReplicaId, Dots): Dotted[Token] = updateWants:
    wants.addElem(replicaId)

  def release(using ReplicaId, Dots): Dotted[Token] = updateWants:
    wants.removeElem(replicaId)

  def grant(using ReplicaId): Token =
    if !isOwner then Token.unchanged
    else
      selectFrom(wants) match
        case None => Token.unchanged
        case Some(nextOwner) =>
          Token(Ownership(os.epoche + 1, nextOwner), ReplicatedSet.empty)

  def selectFrom(wants: ReplicatedSet[Uid])(using ReplicaId) =
    // We find the “largest” ID that wants the token.
    // This is incredibly “unfair” but does prevent deadlocks in case someone needs multiple tokens.
    wants.elements.maxOption.filter(id => id != replicaId)

  def isOwner(using ReplicaId): Boolean = replicaId == os.owner
}

object Token {
  val unchanged: Token = Token(Ownership.unchanged, ReplicatedSet.empty)
  given Lattice[Token] = Lattice.derived
}

case class ExampleTokens(
    calendarAinteractionA: Token,
    calendarBinteractionA: Token
)

case class Vote(owner: Uid, voter: Uid)
case class Voting(rounds: Epoche[ReplicatedSet[Vote]])

object Voting {

  val unchanged: Dotted[Voting] = Dotted(Voting(Epoche.empty))

  given Lattice[Voting] = Lattice.derived

  val participants = 5
  val threshold    = (participants / 2) + 1

  extension [C, E](container: C)
    def voting: syntax[C] = syntax(container)

  implicit class syntax[C](container: C) extends OpsSyntaxHelper[C, Voting](container) {

    def request(using ReplicaId): CausalMutator = delta:
      if !current.rounds.value.isEmpty then unchanged
      else voteFor(replicaId)

    def voteFor(replica: Uid)(using ReplicaId, IsCausalMutator): Dotted[Voting] =
      val newVote = current.rounds.value.inheritContext.add(Vote(replicaId, replicaId))
      newVote.map(rs => Voting(current.rounds.write(rs)))

    def release(using ReplicaId): Mutator = delta:
      Voting(Epoche(current.rounds.counter + 1, ReplicatedSet.empty))

    def vote(using ReplicaId): CausalMutator = delta:
      val (id, count) = leadingCount
      if checkIfMajorityPossible(count)
      then voteFor(id)
      else current.inheritContext.release

    def checkIfMajorityPossible(count: Int)(using IsQuery): Boolean =
      val totalVotes     = current.rounds.value.elements.size
      val remainingVotes = participants - totalVotes
      (count + remainingVotes) > threshold

    def leadingCount(using ReplicaId, IsQuery): (Uid, Int) =
      val votes: Set[Vote] = current.rounds.value.elements
      votes.groupBy(_.owner).map((o, elems) => (o, elems.size)).maxBy((o, size) => size)

    def isOwner(using ReplicaId, IsQuery): Boolean =
      val (id, count) = leadingCount
      id == replicaId && count >= threshold

  }
}
