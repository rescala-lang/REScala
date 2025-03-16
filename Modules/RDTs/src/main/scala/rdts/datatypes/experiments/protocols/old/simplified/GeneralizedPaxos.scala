package rdts.datatypes.experiments.protocols.old.simplified

import rdts.base.LocalUid.replicaId
import rdts.base.{Lattice, LocalUid, Uid}
import rdts.datatypes.LastWriterWins
import rdts.datatypes.experiments.protocols.{BallotNum, Consensus, LeaderElection, Participants, Voting}

case class GeneralizedPaxos[A](
    rounds: Map[BallotNum, (LeaderElection, Voting[A])] =
      Map.empty[BallotNum, (LeaderElection, Voting[A])],
    myValue: Map[Uid, LastWriterWins[A]] = Map.empty
):

  def voteFor(leader: Uid, value: A)(using LocalUid, Participants): (LeaderElection, Voting[A]) =
    (Voting[Uid]().voteFor(leader), Voting[A]().voteFor(value))

  def voteFor(leader: Uid)(using LocalUid, Participants): (LeaderElection, Voting[A]) =
    (Voting[Uid]().voteFor(leader), Voting[A]())

  def phase1a(using LocalUid, Participants): GeneralizedPaxos[A] =
    val nextBallotNum: BallotNum =
      val maxCounter: Long = rounds
        .filter((b, _) => b.uid == replicaId)
        .map((b, _) => b.counter)
        .maxOption
        .getOrElse(-1)
      BallotNum(replicaId, maxCounter + 1)
    GeneralizedPaxos(Map(nextBallotNum -> voteFor(replicaId)))

  def phase1b(using LocalUid, Participants): GeneralizedPaxos[A] =
    // return latest proposed value
    val r              = rounds.filter { case (b, (l, v)) => v.votes.nonEmpty }
    val latestProposal = r.maxByOption { case (b, (l, v)) => b }

    // vote for newest leader election
    val highestBallotNum = highestBallot.map(_._1)
    val leaderCandidate  = highestBallot.map(_._1.uid)

    (highestBallotNum, leaderCandidate, latestProposal) match
      case (Some(ballotNum), Some(candidate), None) => // no value voted for, just vote for candidate
        GeneralizedPaxos(Map(ballotNum -> voteFor(candidate)))
      case (Some(ballotNum), Some(candidate), Some(proposal)) => // vote for candidate and proposed value
        GeneralizedPaxos(Map(
          ballotNum -> voteFor(candidate),
          proposal
        ))
      case _ => GeneralizedPaxos() // do nothing

  def phase2a(using LocalUid, Participants): GeneralizedPaxos[A] =
    // check if leader
    myHighestBallot match
      case Some((ballotNum, (leaderElection, voting))) if leaderElection.result.contains(replicaId) =>
        // find latest value that was voted on
        val latestVal: Option[A] = rounds.filter {
          case (ballotNum, (leaderElection, voting)) => voting.votes.nonEmpty
        }.maxByOption {
          case (ballotNum, (leaderElection, voting)) => ballotNum
        }.map {
          case (ballotNum, (leaderElection, voting)) => voting.votes.head.value
        }
        val value = latestVal.getOrElse(myValue(replicaId).value)
        GeneralizedPaxos(Map(ballotNum -> voteFor(replicaId, value)))
      // not leader -> do nothing
      case _ => GeneralizedPaxos()

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
  def highestBallot: Option[(BallotNum, (LeaderElection, Voting[A]))] = rounds.maxByOption { case (b, (l, v)) =>
    b
  }
  def myHighestBallot(using LocalUid): Option[(BallotNum, (LeaderElection, Voting[A]))] =
    rounds.filter { case (b, (l, v)) => b.uid == replicaId }.maxByOption { case (b, (l, v)) => b }

  def newestDecidedVal(using Participants): Option[A] =
    rounds.collectFirst { case (ballotNum, (leaderElection, voting)) if voting.result.isDefined => voting.result.get }

object GeneralizedPaxos:
  given l[A]: Lattice[GeneralizedPaxos[A]] = Lattice.derived
  given consensus: Consensus[GeneralizedPaxos] with
    extension [A](c: GeneralizedPaxos[A])
      override def propose(value: A)(using LocalUid, Participants): GeneralizedPaxos[A] =
        // check if I can propose a value
        val afterProposal = c.phase2a
        if Lattice.subsumption(afterProposal, c) then
          // proposing did not work, try to become leader
          c.phase1a.copy(myValue = Map(replicaId -> LastWriterWins.now(value)))
        else
          afterProposal
    extension [A](c: GeneralizedPaxos[A])(using Participants)
      override def decision: Option[A] = c.newestDecidedVal
    extension [A](c: GeneralizedPaxos[A])
      override def upkeep()(using LocalUid, Participants): GeneralizedPaxos[A] =
        // check which phase we are in
        c.highestBallot match
          // we have a leader -> phase 2
          case Some((ballotNum, (leaderElection, voting))) if leaderElection.result.nonEmpty =>
            if leaderElection.result.get == replicaId then
              c.phase2a
            else
              c.phase2b
          // we are in the process of electing a new leader
          case _ =>
            c.phase1b

    override def empty[A]: GeneralizedPaxos[A] = GeneralizedPaxos()

    override def lattice[A]: Lattice[GeneralizedPaxos[A]] = l

    // TODO: define lteq
