package rdts.datatypes.experiments.protocols.simplified

import rdts.base.LocalUid.replicaId
import rdts.base.{Bottom, Lattice, LocalUid, Uid}
import rdts.datatypes.experiments.protocols.Participants
import rdts.datatypes.experiments.protocols.Participants.participants
import rdts.datatypes.{Epoch, GrowOnlyMap, GrowOnlySet}

case class Vote[A](value: A, voter: Uid)

case class SimpleVoting[A](votes: Set[Vote[A]] = Set.empty[Vote[A]]) {
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

object SimpleVoting {
  given lattice[A]: Lattice[SimpleVoting[A]] = Lattice.derived

  given bottom[A](using Participants): Bottom[SimpleVoting[A]] with
    override def empty: SimpleVoting[A] = unchanged

  def unchanged[A](using Participants): SimpleVoting[A] = SimpleVoting(Set.empty)
}

type LeaderElection = SimpleVoting[Uid]

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

object MultiRoundVoting {
  def unchanged[A](using Participants): MultiRoundVoting[A] = MultiRoundVoting(Epoch.empty[SimpleVoting[A]])
  given lattice[A]: Lattice[MultiRoundVoting[A]]            = Lattice.derived
}

case class BallotNum(uid: Uid, counter: Long)

given Ordering[BallotNum] with
  override def compare(x: BallotNum, y: BallotNum): Int =
    if x.counter > y.counter then 1
    else if x.counter < y.counter then -1
    else Ordering[Uid].compare(x.uid, y.uid)

case class GeneralizedPaxos[A](rounds: Map[BallotNum, (LeaderElection, SimpleVoting[A])] =
  Map.empty[BallotNum, (LeaderElection, SimpleVoting[A])]):

  def voteFor(leader: Uid, value: A)(using LocalUid, Participants): (LeaderElection, SimpleVoting[A]) =
    (SimpleVoting[Uid]().voteFor(leader), SimpleVoting[A]().voteFor(value))

  def voteFor(leader: Uid)(using LocalUid, Participants): (LeaderElection, SimpleVoting[A]) =
    (SimpleVoting[Uid]().voteFor(leader), SimpleVoting[A]())

  def phase1a(using LocalUid, Participants): GeneralizedPaxos[A] =
    val nextBallotNum: BallotNum = ???
    GeneralizedPaxos(Map(nextBallotNum -> voteFor(replicaId)))

  def phase1b(using LocalUid, Participants): GeneralizedPaxos[A] =
    // return greatest decided value
    val r          = rounds.filter { case (b, (l, v)) => v.result != None }
    val decidedVal = r.maxByOption { case (b, (l, v)) => b }.flatMap(_._2._2.result)

    // vote for newest leader election
    val highestBallotNum = highestBallot.map(_._1)
    val leaderCandidate  = highestBallot.map(_._1.uid)

    (highestBallotNum, leaderCandidate, decidedVal) match
      case (Some(ballotNum), Some(candidate), None) => // no value decided, just vote for candidate
        GeneralizedPaxos(Map(ballotNum -> voteFor(candidate)))
      case (Some(ballotNum), Some(candidate), Some(decidedValue)) => // vote for candidate and decided value
        GeneralizedPaxos(Map(ballotNum -> voteFor(candidate, decidedValue)))
      case _ => GeneralizedPaxos() // do nothing

  def phase2a(using LocalUid, Participants): GeneralizedPaxos[A] =
    // check if leader
    myHighestBallot match
      case Some((ballotNum, (leaderElection, voting))) if leaderElection.result == Some(replicaId) =>
        val value =
          if voting.votes.nonEmpty then
            voting.votes.head.value
          else ??? // TODO: get value from context?
        GeneralizedPaxos(Map(ballotNum -> voteFor(replicaId, value)))
      case None => GeneralizedPaxos()

  def phase2b(using LocalUid, Participants): GeneralizedPaxos[A] =
    // get highest ballot with leader
    val roundsWithLeaders = rounds.filter { case (ballotNum, (leaderElection, voting)) =>
      leaderElection.result.nonEmpty
    }
    val highestBallot = roundsWithLeaders.maxByOption { case (ballotNum, (leaderElection, voting)) => ballotNum }
    val newestLeader: Option[Uid] = highestBallot
      .flatMap { case (ballotNum, (leaderElection, voting)) => leaderElection.result }
    val proposedValue: Option[A] = highestBallot
      .flatMap { case (ballotNum, (leaderElection, voting)) => voting.votes.headOption.map(_.value) }

    (highestBallot.map(_._1), newestLeader, proposedValue) match
      // if leader and value are known, vote for it, otherwise do nothing
      case (Some(ballotNum), Some(leader), Some(value)) =>
        GeneralizedPaxos(Map(ballotNum -> voteFor(leader, value)))
      case _ => GeneralizedPaxos()

  // helpers
  def highestBallot: Option[(BallotNum, (LeaderElection, SimpleVoting[A]))] = rounds.maxByOption { case (b, (l, v)) =>
    b
  }
  def myHighestBallot(using LocalUid): Option[(BallotNum, (LeaderElection, SimpleVoting[A]))] =
    rounds.filter { case (b, (l, v)) => b.uid == replicaId }.maxByOption { case (b, (l, v)) => b }
