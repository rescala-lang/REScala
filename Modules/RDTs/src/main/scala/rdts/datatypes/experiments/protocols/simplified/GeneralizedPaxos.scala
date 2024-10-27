package rdts.datatypes.experiments.protocols.simplified

import rdts.base.LocalUid.replicaId
import rdts.base.{Lattice, LocalUid, Uid}
import rdts.datatypes.LastWriterWins
import rdts.datatypes.experiments.protocols.{Consensus, Participants}

case class GeneralizedPaxos[A](
    rounds: Map[BallotNum, (LeaderElection, SimpleVoting[A])] =
      Map.empty[BallotNum, (LeaderElection, SimpleVoting[A])],
    myValue: Option[LastWriterWins[A]] = None
):

  def voteFor(leader: Uid, value: A)(using LocalUid, Participants): (LeaderElection, SimpleVoting[A]) =
    (SimpleVoting[Uid]().voteFor(leader), SimpleVoting[A]().voteFor(value))

  def voteFor(leader: Uid)(using LocalUid, Participants): (LeaderElection, SimpleVoting[A]) =
    (SimpleVoting[Uid]().voteFor(leader), SimpleVoting[A]())

  def phase1a(using LocalUid, Participants): GeneralizedPaxos[A] =
    val nextBallotNum: BallotNum = ???
    GeneralizedPaxos(Map(nextBallotNum -> voteFor(replicaId)))

  def phase1b(using LocalUid, Participants): GeneralizedPaxos[A] =
    // return greatest decided value
    val r          = rounds.filter { case (b, (l, v)) => v.result.isDefined }
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
      case Some((ballotNum, (leaderElection, voting))) if leaderElection.result.contains(replicaId) =>
        val value =
          if voting.votes.nonEmpty then
            voting.votes.head.value
          else myValue.get.value // get my value from context
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

  def newestDecidedVal(using Participants): Option[A] =
    val r = rounds.filter { case (b, (l, v)) => v.result.isDefined }
    r.maxByOption { case (b, (l, v)) => b }.flatMap(_._2._2.result)

object GeneralizedPaxos:
  given lattice[A]: Lattice[GeneralizedPaxos[A]] = Lattice.derived
  given consensus: Consensus[GeneralizedPaxos] with
    extension [A](c: GeneralizedPaxos[A])
      override def write(value: A)(using LocalUid, Participants): GeneralizedPaxos[A] =
        val withValue = c.copy(myValue = Some(LastWriterWins.now(value)))
        // check if I can propose a value
        val afterProposal = withValue.phase2a
        if Lattice[GeneralizedPaxos[A]].lteq(afterProposal, withValue) then
          // proposing did not work, try to become leader
          withValue.phase1a
        else
          afterProposal
    extension [A](c: GeneralizedPaxos[A])(using Participants)
      override def read: Option[A] = c.newestDecidedVal
    extension [A](c: GeneralizedPaxos[A])
      override def upkeep()(using LocalUid, Participants): GeneralizedPaxos[A] =
        // check which phase we are in
        c.highestBallot match
          // we have a leader -> phase 2
          case Some((ballotNum, (leaderElection, voting))) if leaderElection.result.nonEmpty =>
            c.phase2b
          // we are in the process of electing a new leader
          case None =>
            c.phase1b

    override def empty[A]: GeneralizedPaxos[A] = GeneralizedPaxos()

    override def lattice[A]: Lattice[GeneralizedPaxos[A]] = lattice

    // TODO: define lteq
