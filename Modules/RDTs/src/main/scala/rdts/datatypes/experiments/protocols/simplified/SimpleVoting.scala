package rdts.datatypes.experiments.protocols.simplified

import rdts.base.LocalUid.replicaId
import rdts.base.{Bottom, Lattice, LocalUid, Uid}
import rdts.datatypes.experiments.protocols.simplified.Participants.participants
import rdts.datatypes.{Epoch, GrowOnlySet}

case class Vote[A](value: A, voter: Uid)

val numParticipants = 4

case class SimpleVoting[A](votes: Set[Vote[A]]) {
  def threshold(using Participants): Int = participants.size / 2 + 1

  def result(using Participants): Option[A] =
    leadingCount match
      case Some((v, count)) if count >= threshold => Some(v)
      case _                                      => None

  def voteFor(v: A)(using LocalUid, Participants): SimpleVoting[A] =
    if !participants.contains(replicaId) || votes.exists { case Vote(_, voter) => voter == replicaId }
    then SimpleVoting.unchanged // already voted!
    else
      SimpleVoting(Set(Vote(v, replicaId)))

  def leadingCount: Option[(A, Int)] =
    val grouped: Map[A, Int] = votes.groupBy(_.value).map((value, vts) => (value, vts.size))
    grouped.maxByOption((_, size) => size)
}

type LeaderElection = SimpleVoting[Uid]

case class Participants(members: Set[Uid])

object Participants:
  def participants(using p: Participants): Set[Uid] =
    p.members

case class MultiRoundVoting[A](rounds: Epoch[SimpleVoting[A]]):
  def release(using Participants): MultiRoundVoting[A] =
    MultiRoundVoting(Epoch(rounds.counter + 1, SimpleVoting.unchanged))

  def upkeep(using LocalUid, Participants): MultiRoundVoting[A] =
    rounds.value.leadingCount match
      case Some(value, count) if checkIfMajorityPossible => voteFor(value)
      case Some(_) => release                    // we have a leading proposal but majority is not possible anymore
      case None    => MultiRoundVoting.unchanged // no change yet

  def checkIfMajorityPossible(using Participants): Boolean =
    val totalVotes     = rounds.value.votes.size
    val remainingVotes = participants.size - totalVotes
    val possible       = rounds.value.leadingCount.map((_, count) => (count + remainingVotes) >= rounds.value.threshold)
    possible.getOrElse(true) // if there is no leading vote, majority is always possible

  // api
  def voteFor(c: A)(using LocalUid, Participants): MultiRoundVoting[A] =
    MultiRoundVoting(Epoch(rounds.counter, rounds.value.voteFor(c)))

  def result(using Participants): Option[A] =
    rounds.value.result

object SimpleVoting {
  given lattice[A]: Lattice[SimpleVoting[A]] = Lattice.derived
  given bottom[A](using Participants): Bottom[SimpleVoting[A]] with
    override def empty: SimpleVoting[A] = unchanged
  def unchanged[A](using Participants): SimpleVoting[A] = SimpleVoting(Set.empty)
}
object MultiRoundVoting {
  def unchanged[A](using Participants): MultiRoundVoting[A] = MultiRoundVoting(Epoch.empty[SimpleVoting[A]])
  given lattice[A]: Lattice[MultiRoundVoting[A]]            = Lattice.derived
}
