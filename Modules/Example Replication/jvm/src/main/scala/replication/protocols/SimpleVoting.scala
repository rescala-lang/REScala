package replication.protocols.simplified
import rdts.base.{Bottom, Lattice, Uid}
import rdts.datatypes.GrowOnlySet.syntax
import rdts.datatypes.{Epoch, GrowOnlySet}
import rdts.syntax.LocalUid
import rdts.syntax.LocalUid.replicaId

val numParticipants = 4

case class Vote(leader: Uid, voter: Uid)
case class Voting(votes: Set[Vote]) {
  def threshold: Int = numParticipants / 2 + 1

  def isLeader(using LocalUid): Boolean =
    val (id, count) = leadingCount
    id == replicaId && count >= threshold

  def voteFor(uid: Uid)(using LocalUid): Voting =
    if votes.elements.exists { case Vote(_, voter) => voter == replicaId }
    then Voting(Set.empty) // already voted!
    else
      Voting(Set(Vote(uid, replicaId)))

  def leadingCount(using id: LocalUid): (Uid, Int) =
    val grouped: Map[Uid, Int] = votes.groupBy(_.leader).map((o, elems) => (o, elems.size))
    if grouped.isEmpty
    then (replicaId, 0)
    else grouped.maxBy((o, size) => size)
}

case class MultiRoundVoting(rounds: Epoch[Voting]):
  def release: MultiRoundVoting =
    MultiRoundVoting(Epoch(rounds.counter + 1, Voting(Set.empty)))

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

object Voting {
  given Lattice[Voting] = Lattice.derived
  given Bottom[Voting] with
    override def empty: Voting = unchanged
  def unchanged: Voting = Voting(GrowOnlySet.empty)
}
object MultiRoundVoting {
  def unchanged: MultiRoundVoting = MultiRoundVoting(Epoch.empty[Voting])
  given Lattice[MultiRoundVoting] = Lattice.derived
}
