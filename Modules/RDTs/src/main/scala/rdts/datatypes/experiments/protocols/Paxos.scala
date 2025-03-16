package rdts.datatypes.experiments.protocols

import rdts.base.LocalUid.replicaId
import rdts.base.{Bottom, Lattice, LocalUid, Uid}
import rdts.datatypes.LastWriterWins
import rdts.datatypes.experiments.protocols.Paxos.given
import rdts.datatypes.experiments.protocols.{Consensus, Participants}

case class PaxosRound[A](leaderElection: LeaderElection = Voting(), proposals: Voting[A] = Voting[A]())
type LeaderElection = Voting[Uid]
type Proposal

case class Paxos[A](
    rounds: Map[BallotNum, PaxosRound[A]] =
      Map.empty[BallotNum, PaxosRound[A]]
):
  // update functions
  def voteLeader(leader: Uid)(using LocalUid, Participants): PaxosRound[A] =
    PaxosRound(leaderElection = Voting().voteFor(leader))

  def voteValue(value: A)(using LocalUid, Participants): PaxosRound[A] =
    PaxosRound(proposals = Voting().voteFor(value))

  // query functions
  def nextBallotNum(using LocalUid): BallotNum =
    val maxCounter: Long = rounds
//      .filter((b, _) => b.uid == replicaId)
      .map((b, _) => b.counter)
      .maxOption
      .getOrElse(-1)
    BallotNum(replicaId, maxCounter + 1)
  def currentRound: Option[(BallotNum, PaxosRound[A])] = rounds.maxOption
  def currentBallot: Option[BallotNum]                 = rounds.maxOption.map(_._1)
  def newestBallotWithLeader(using Participants): Option[(BallotNum, PaxosRound[A])] =
    rounds.filter(_._2.leaderElection.result.nonEmpty).maxOption
  def currentLeaderElection: Option[LeaderElection] = currentRound.map(_._2.leaderElection)
  def leaderCandidate: Option[Uid]                  = currentLeaderElection.map(_.votes.head.value)
  def myHighestBallot(using LocalUid): Option[(BallotNum, PaxosRound[A])] =
    rounds.filter { case (b, p) => b.uid == replicaId }.maxOption
  def lastValueVote: Option[(BallotNum, PaxosRound[A])] = rounds.filter(_._2.proposals.votes.nonEmpty).maxOption
  def newestReceivedVal(using LocalUid)                 = lastValueVote.map(_._2.proposals.votes.head.value)
  def myValue(using LocalUid): Option[A] =
    rounds.get(BallotNum(replicaId, -1)).map(_.proposals.votes.head.value)
  def decidedVal(using Participants): Option[A] =
    rounds.collectFirst { case (b, PaxosRound(_, voting)) if voting.result.isDefined => voting.result.get }
  def decidedLeaderElection(using Participants): Option[(BallotNum, LeaderElection)] =
    rounds.collectFirst {
      case (b, PaxosRound(leaderElection, voting)) if voting.result.isDefined => (b, leaderElection)
    }

  // phases:
  def phase1a(using LocalUid, Participants)(value: A): Paxos[A] =
    // try to become leader and remember a value for later
    Paxos(Map(nextBallotNum -> voteLeader(replicaId), BallotNum(replicaId, -1) -> voteValue(value)))
  def phase1a(using LocalUid, Participants): Paxos[A] =
    // try to become leader
    Paxos(Map(nextBallotNum -> voteLeader(replicaId)))

  def phase1b(using LocalUid, Participants): Paxos[A] =
    // vote in the current leader election
    (currentBallot, leaderCandidate, lastValueVote) match
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
    // try to determine my process' value from previous ballot
    myValue match
      case Some(value) => phase2a(value)
      case None        => Paxos()

  def phase2a(value: A)(using LocalUid, Participants): Paxos[A] =
    // propose a value if I am the leader
    myHighestBallot match
      case Some((ballotNum, PaxosRound(leaderElection, _)))
          if leaderElection.result.contains(replicaId) =>
        if newestReceivedVal.nonEmpty then
          Paxos(Map(ballotNum -> voteValue(newestReceivedVal.get)))
        else
          Paxos(Map(ballotNum -> voteValue(value)))
      case _ => Paxos() // not leader -> do nothing

  def phase2b(using LocalUid, Participants): Paxos[A] =
    // get highest ballot with leader and
    // vote for the proposed value if there is one
    newestBallotWithLeader match
      case Some(ballotNum, PaxosRound(leaderElection, proposals))
          if proposals.votes.nonEmpty =>
        Paxos(Map(ballotNum -> voteValue(proposals.votes.head.value)))
      case _ => Paxos() // current leader or proposed value not known -> do nothing

object Paxos:
  given [A]: Lattice[PaxosRound[A]] = Lattice.derived
  given l[A]: Lattice[Paxos[A]]     = Lattice.derived

  given [A]: Bottom[Paxos[A]] = Bottom.provide(Paxos())

  given [A]: Ordering[(BallotNum, PaxosRound[A])] with
    override def compare(x: (BallotNum, PaxosRound[A]), y: (BallotNum, PaxosRound[A])): Int = (x, y) match
      case ((x, _), (y, _)) => Ordering[BallotNum].compare(x, y)

  given consensus: Consensus[Paxos] with
    extension [A](c: Paxos[A])
      override def propose(value: A)(using LocalUid, Participants): Paxos[A] =
        // check if I can propose a value
        val afterProposal = c.phase2a
        if Lattice.subsumption(afterProposal, c) then
          // proposing did not work, try to become leader
          c.phase1a(value)
        else
          afterProposal
    extension [A](c: Paxos[A])(using Participants)
      override def decision: Option[A] = c.decidedVal
    extension [A](c: Paxos[A])
      // upkeep can be used to perform the next protocol step automatically
      override def upkeep()(using LocalUid, Participants): Paxos[A] =
        // check which phase we are in
        c.currentRound match
          case Some((ballotNum, PaxosRound(leaderElection, _))) if leaderElection.result.nonEmpty =>
            // we have a leader -> phase 2
            if leaderElection.result.get == replicaId then
              c.phase2a
            else
              c.phase2b
          // we are in the process of electing a new leader
          case _ =>
            c.phase1b

    override def empty[A]: Paxos[A] = Paxos()

    override def lattice[A]: Lattice[Paxos[A]] = l
