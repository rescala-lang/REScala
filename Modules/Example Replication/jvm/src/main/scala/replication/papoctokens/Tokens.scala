package replication.papoctokens

import rdts.base.{Bottom, Lattice, Orderings, Uid}
import rdts.datatypes.Epoch
import rdts.datatypes.contextual.ReplicatedSet
import rdts.datatypes.contextual.ReplicatedSet.syntax
import rdts.datatypes.experiments.RaftState
import rdts.dotted.{Dotted, HasDots}
import rdts.syntax.LocalReplicaId.replicaId
import rdts.syntax.{DeltaBuffer, OpsSyntaxHelper, LocalReplicaId}
import rdts.time.Dots

import scala.util.Random

case class Ownership(epoch: Long, owner: Uid)

object Ownership {
  given Lattice[Ownership] = Lattice.fromOrdering(using Orderings.lexicographic)

  given bottom: Bottom[Ownership] = Bottom.provide(Ownership(Long.MinValue, Uid.zero))

  def unchanged: Ownership = bottom.empty
}

case class Token(os: Ownership, wants: ReplicatedSet[Uid]) {

  def isOwner(using LocalReplicaId): Boolean = replicaId == os.owner

  def request(using LocalReplicaId, Dots): Dotted[Token] =
    wants.addElem(replicaId).map(w => Token(Ownership.unchanged, w))

  def release(using LocalReplicaId, Dots): Dotted[Token] =
    wants.removeElem(replicaId).map(w => Token(Ownership.unchanged, w))

  def upkeep(using LocalReplicaId): Token =
    if !isOwner then Token.unchanged
    else
      selectFrom(wants) match
        case None => Token.unchanged
        case Some(nextOwner) =>
          Token(Ownership(os.epoch + 1, nextOwner), ReplicatedSet.empty)

  def selectFrom(wants: ReplicatedSet[Uid])(using LocalReplicaId) =
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

case class Vote(owner: Uid, voter: Uid)
case class Voting(rounds: Epoch[ReplicatedSet[Vote]]) {

  def isOwner(using LocalReplicaId): Boolean =
    val (id, count) = leadingCount
    id == replicaId && count >= Voting.threshold

  def request(using LocalReplicaId, Dots): Dotted[Voting] =
    if !rounds.value.isEmpty then Voting.unchanged
    else voteFor(replicaId)

  def release(using LocalReplicaId): Voting =
    if !isOwner
    then Voting.unchanged.data
    else Voting(Epoch(rounds.counter + 1, ReplicatedSet.empty))

  def upkeep(using LocalReplicaId, Dots): Dotted[Voting] =
    val (id, count) = leadingCount
    if checkIfMajorityPossible(count)
    then voteFor(id)
    else Dotted(forceRelease)

  def forceRelease(using LocalReplicaId): Voting =
    Voting(Epoch(rounds.counter + 1, ReplicatedSet.empty))

  def voteFor(uid: Uid)(using LocalReplicaId, Dots): Dotted[Voting] =
    if rounds.value.elements.exists { case Vote(id, _) => id == replicaId }
    then Voting.unchanged // already voted!
    else
      val newVote = rounds.value.addElem(Vote(uid, replicaId))
      newVote.map(rs => Voting(rounds.write(rs)))

  def checkIfMajorityPossible(count: Int): Boolean =
    val totalVotes     = rounds.value.elements.size
    val remainingVotes = Voting.participants - totalVotes
    (count + remainingVotes) > Voting.threshold

  def leadingCount(using LocalReplicaId): (Uid, Int) =
    val votes: Set[Vote] = rounds.value.elements
    votes.groupBy(_.owner).map((o, elems) => (o, elems.size)).maxBy((o, size) => size)
}

object Voting {

  val unchanged: Dotted[Voting] = Dotted(Voting(Epoch.empty))

  given Lattice[Voting] = Lattice.derived

  val participants = 5
  val threshold    = (participants / 2) + 1

}

case class Exclusive[T: Lattice: Bottom](token: Token, value: T) {
  def transform(f: T => T)(using LocalReplicaId) =
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
