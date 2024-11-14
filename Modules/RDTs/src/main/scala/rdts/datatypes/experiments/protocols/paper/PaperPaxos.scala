package rdts.datatypes.experiments.protocols.paper

import rdts.base.LocalUid.replicaId
import rdts.base.{Lattice, LocalUid, Uid}
import rdts.datatypes.LastWriterWins
import rdts.datatypes.experiments.protocols.simplified.{BallotNum, Voting}
import rdts.datatypes.experiments.protocols.simplified.given_Ordering_BallotNum
import rdts.datatypes.experiments.protocols.{Consensus, Participants}
import Paxos.given

case class PaxosRound[A](leaderElection: LeaderElection = Voting(), proposals: Voting[A] = Voting[A]())
type LeaderElection = Voting[Uid]
type Proposal

case class Paxos[A](
    rounds: Map[BallotNum, PaxosRound[A]] =
      Map.empty[BallotNum, PaxosRound[A]]
):
  // update functions
  def nextBallotNum(using LocalUid): BallotNum =
    val maxCounter: Long = rounds
      .filter((b, _) => b.uid == replicaId)
      .map((b, _) => b.counter)
      .maxOption
      .getOrElse(-1)
    BallotNum(replicaId, maxCounter + 1)

  def voteFor(leader: Uid, value: A)(using LocalUid, Participants): (LeaderElection, Voting[A]) =
    (Voting[Uid]().voteFor(leader), Voting[A]().voteFor(value))

  def voteLeader(leader: Uid)(using LocalUid, Participants): PaxosRound[A] =
    PaxosRound(leaderElection = Voting().voteFor(leader))

  def voteValue(value: A)(using LocalUid, Participants): PaxosRound[A] =
    PaxosRound(proposals = Voting().voteFor(value))

  // query functions
  def currentRound: Option[(BallotNum, PaxosRound[A])] = rounds.maxOption
  def currentBallot: Option[BallotNum]                 = rounds.maxOption.map(_._1)
  def newestBallotWithLeader(using Participants): Option[(BallotNum, PaxosRound[A])] =
    rounds.filter(_._2.leaderElection.result.nonEmpty).maxOption

  def currentLeaderElection: Option[LeaderElection] = currentRound.map(_._2.leaderElection)
  def leaderCandidate: Option[Uid]                  = currentLeaderElection.map(_.votes.head.value)

  def myHighestBallot(using LocalUid): Option[(BallotNum, PaxosRound[A])] =
    rounds.filter { case (b, p) => b.uid == replicaId }.maxOption

  def lastVotedVal: Option[(BallotNum, PaxosRound[A])] = rounds.filter(_._2.proposals.votes.nonEmpty).maxOption
  def lastVotedValOrMyValue = lastVotedVal.map(_._2.proposals.votes.head).getOrElse(myValue).value
  def myValue               = ???

  // phases:
  def phase1a(using LocalUid, Participants): Paxos[A] =
    // try to become leader
    Paxos(Map(nextBallotNum -> voteLeader(replicaId)))

  def phase1b(using LocalUid, Participants): Paxos[A] =
    // vote in the current leader election
    (currentBallot, leaderCandidate, lastVotedVal) match
      case (Some(ballotNum), Some(candidate), None) =>
        // no value voted for, just vote for candidate
        Paxos(Map(ballotNum -> voteLeader(candidate)))
      case (Some(ballotNum), Some(candidate), Some(votedVal)) =>
        // vote for candidate and include value most recently voted for
        Paxos(Map(
          ballotNum -> voteLeader(candidate),
          votedVal
        ))
      case _ => Paxos() // do nothing

  def phase2a(using LocalUid, Participants): Paxos[A] =
    // propose a value if I am the leader
    myHighestBallot match
      case Some((ballotNum, PaxosRound(leaderElection, _)))
          if leaderElection.result.contains(replicaId) =>
        Paxos(Map(ballotNum -> voteValue(lastVotedValOrMyValue)))
      case None => Paxos() // not leader -> do nothing

  def phase2b(using LocalUid, Participants): Paxos[A] =
    // get highest ballot with leader and
    // vote for the proposed value if there is one
    newestBallotWithLeader match
      case Some(ballotNum, PaxosRound(leaderElection, proposals))
          if proposals.votes.nonEmpty =>
        Paxos(Map(ballotNum -> voteValue(proposals.votes.head.value)))
      case _ => Paxos() // current leader or proposed value not known -> do nothing

object Paxos:
  given l[A]: Lattice[Paxos[A]] = Lattice.derived

  given [A]: Ordering[(BallotNum, PaxosRound[A])] with
    override def compare(x: (BallotNum, PaxosRound[A]), y: (BallotNum, PaxosRound[A])): Int = (x, y) match
      case ((x, _), (y, _)) => Ordering[BallotNum].compare(x, y)
