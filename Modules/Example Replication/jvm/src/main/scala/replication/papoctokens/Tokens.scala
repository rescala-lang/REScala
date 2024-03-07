package replication.papoctokens

import kofre.base.{Bottom, Lattice, Orderings, Uid}
import kofre.datatypes.Epoch
import kofre.datatypes.contextual.ReplicatedSet
import kofre.datatypes.contextual.ReplicatedSet.syntax
import kofre.datatypes.experiments.RaftState
import kofre.dotted.{Dotted, HasDots}
import kofre.syntax.ReplicaId.replicaId
import kofre.syntax.{DeltaBuffer, OpsSyntaxHelper, ReplicaId}
import kofre.time.Dots

import scala.util.Random

case class Ownership(epoch: Long, owner: Uid)

object Ownership {
  given Lattice[Ownership] = Lattice.fromOrdering(using Orderings.lexicographic)

  given bottom: Bottom[Ownership] = Bottom.provide(Ownership(Long.MinValue, Uid.zero))

  def unchanged: Ownership = bottom.empty
}

case class Token(os: Ownership, wants: ReplicatedSet[Uid]) {

  def isOwner(using ReplicaId): Boolean = replicaId == os.owner

  def request(using ReplicaId, Dots): Dotted[Token] =
    wants.addElem(replicaId).map(w => Token(Ownership.unchanged, w))

  def release(using ReplicaId, Dots): Dotted[Token] =
    wants.removeElem(replicaId).map(w => Token(Ownership.unchanged, w))

  def upkeep(using ReplicaId): Token =
    if !isOwner then Token.unchanged
    else
      selectFrom(wants) match
        case None => Token.unchanged
        case Some(nextOwner) =>
          Token(Ownership(os.epoch + 1, nextOwner), ReplicatedSet.empty)

  def selectFrom(wants: ReplicatedSet[Uid])(using ReplicaId) =
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

  def isOwner(using ReplicaId): Boolean =
    val (id, count) = leadingCount
    id == replicaId && count >= Voting.threshold

  def request(using ReplicaId, Dots): Dotted[Voting] =
    if !rounds.value.isEmpty then Voting.unchanged
    else voteFor(replicaId)

  def release(using ReplicaId): Voting =
    if !isOwner
    then Voting.unchanged.data
    else Voting(Epoch(rounds.counter + 1, ReplicatedSet.empty))

  def upkeep(using ReplicaId, Dots): Dotted[Voting] =
    val (id, count) = leadingCount
    if checkIfMajorityPossible(count)
    then voteFor(id)
    else Dotted(forceRelease)

  def forceRelease(using ReplicaId): Voting =
    Voting(Epoch(rounds.counter + 1, ReplicatedSet.empty))

  def voteFor(uid: Uid)(using ReplicaId, Dots): Dotted[Voting] =
    if rounds.value.elements.exists { case Vote(id, _) => id == replicaId }
    then Voting.unchanged // already voted!
    else
      val newVote = rounds.value.addElem(Vote(uid, replicaId))
      newVote.map(rs => Voting(rounds.write(rs)))

  def checkIfMajorityPossible(count: Int): Boolean =
    val totalVotes     = rounds.value.elements.size
    val remainingVotes = Voting.participants - totalVotes
    (count + remainingVotes) > Voting.threshold

  def leadingCount(using ReplicaId): (Uid, Int) =
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
  def transform(f: T => T)(using ReplicaId) =
    if token.isOwner then f(value) else Bottom.empty
}


/** totally not incredibly inefficient */
case class Causal[T: Lattice: HasDots: Bottom](deltas: Set[Dotted[T]]) {
  def value: T =
    val causalPrefix = deltas.map(_.context).reduceOption(_ union _).map(_.causalPrefix).getOrElse(Dots.empty)
    deltas.filter(delta => delta.context <= causalPrefix).reduceOption(Dotted.lattice.merge).map(_.data).getOrElse(Bottom.empty)
}
