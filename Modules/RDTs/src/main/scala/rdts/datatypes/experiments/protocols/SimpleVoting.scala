package rdts.datatypes.experiments.protocols

import rdts.base.{Bottom, Lattice, Uid}
import rdts.datatypes.GrowOnlySet.syntax
import rdts.datatypes.{Epoch, GrowOnlySet}
import rdts.syntax.LocalUid
import rdts.syntax.LocalUid.replicaId

val numParticipants = 4

case class SimpleVoting(votes: Set[Vote]) {
  def threshold: Int = numParticipants / 2 + 1

  def isLeader(using LocalUid): Boolean =
    val (id, count) = leadingCount
    id == replicaId && count >= threshold

  def voteFor(uid: Uid)(using LocalUid): SimpleVoting =
    if votes.elements.exists { case Vote(_, voter) => voter == replicaId }
    then SimpleVoting(Set.empty) // already voted!
    else
      SimpleVoting(Set(Vote(uid, replicaId)))

  def leadingCount(using id: LocalUid): (Uid, Int) =
    val grouped: Map[Uid, Int] = votes.groupBy(_.leader).map((o, elems) => (o, elems.size))
    if grouped.isEmpty
    then (replicaId, 0)
    else grouped.maxBy((o, size) => size)
}

case class MultiRoundVoting(rounds: Epoch[SimpleVoting]):
  def release: MultiRoundVoting =
    MultiRoundVoting(Epoch(rounds.counter + 1, SimpleVoting(Set.empty)))

  def upkeep(using LocalUid): MultiRoundVoting =
    val (id, count) = rounds.value.leadingCount
    if checkIfMajorityPossible
    then voteFor(id)
    else release

  def checkIfMajorityPossible(using localUid: LocalUid): Boolean =
    val (id, count)    = rounds.value.leadingCount
    val totalVotes     = rounds.value.votes.size
    val remainingVotes = numParticipants - totalVotes
    (count + remainingVotes) > rounds.value.threshold

  // api
  def voteFor(uid: Uid)(using LocalUid): MultiRoundVoting =
    MultiRoundVoting(Epoch(rounds.counter, rounds.value.voteFor(uid)))

  def isLeader(using LocalUid): Boolean =
    rounds.value.isLeader

object SimpleVoting {
  given Lattice[SimpleVoting] = Lattice.derived
  given Bottom[SimpleVoting] with
    override def empty: SimpleVoting = unchanged
  def unchanged: SimpleVoting = SimpleVoting(GrowOnlySet.empty)
}
object MultiRoundVoting {
  def unchanged: MultiRoundVoting = MultiRoundVoting(Epoch.empty[SimpleVoting])
  given Lattice[MultiRoundVoting] = Lattice.derived
}
