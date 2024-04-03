package replication.protocols

import rdts.datatypes.contextual.ReplicatedSet
import rdts.datatypes.{Epoch, LastWriterWins}
import rdts.base.{Bottom, Lattice, Orderings, Uid}
import rdts.dotted.Dotted
import rdts.syntax.LocalReplicaId.replicaId
import rdts.time.Dots
import rdts.syntax.LocalReplicaId
import rdts.syntax.LocalReplicaId.replicaId

case class Vote(owner: Uid, voter: Uid)

case class Voting(rounds: Epoch[ReplicatedSet[Vote]], numParticipants: LastWriterWins[Int]) {
  def threshold: Int = numParticipants.value / 2 + 1

  def isOwner(using LocalReplicaId): Boolean =
    val (id, count) = leadingCount
    id == replicaId && count >= threshold

  def request(using LocalReplicaId, Dots): Dotted[Voting] =
    if !rounds.value.isEmpty then Voting.unchanged
    else voteFor(replicaId)

  def release(using LocalReplicaId): Voting =
    if !isOwner
    then Voting.unchanged.data
    else Voting(Epoch(rounds.counter + 1, ReplicatedSet.empty), numParticipants)

  def upkeep(using LocalReplicaId, Dots): Dotted[Voting] =
    val (id, count) = leadingCount
    if checkIfMajorityPossible(count)
    then voteFor(id)
    else Dotted(forceRelease)

  def forceRelease(using LocalReplicaId): Voting =
    Voting(Epoch(rounds.counter + 1, ReplicatedSet.empty), numParticipants)

  def voteFor(uid: Uid)(using LocalReplicaId, Dots): Dotted[Voting] =
    if rounds.value.elements.exists { case Vote(_, voter) => voter == replicaId }
    then Voting.unchanged // already voted!
    else
      val newVote = rounds.value.addElem(Vote(uid, replicaId))
      newVote.map(rs => Voting(rounds.write(rs), numParticipants))

  def checkIfMajorityPossible(count: Int): Boolean =
    val totalVotes     = rounds.value.elements.size
    val remainingVotes = numParticipants.value - totalVotes
    (count + remainingVotes) > threshold

  def leadingCount(using id: LocalReplicaId): (Uid, Int) =
    val votes: Set[Vote]       = rounds.value.elements
    val grouped: Map[Uid, Int] = votes.groupBy(_.owner).map((o, elems) => (o, elems.size))
    if grouped.isEmpty
    then (replicaId, 0)
    else grouped.maxBy((o, size) => size)
  // .maxBy((o, size) => size)
}

object Voting {
  given Bottom[Int] with
    def empty = 0
  val unchanged: Dotted[Voting] = Dotted(Voting(Epoch.empty, LastWriterWins.empty[Int]))

  given Lattice[Voting] = Lattice.derived

}
