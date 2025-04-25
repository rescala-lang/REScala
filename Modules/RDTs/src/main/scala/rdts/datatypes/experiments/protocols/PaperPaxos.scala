package rdts.datatypes.experiments.protocols.paper

import rdts.base.{Bottom, Lattice, LocalUid, Uid}
import rdts.base.LocalUid.replicaId
import rdts.datatypes.experiments.protocols
import rdts.datatypes.experiments.protocols.Participants.participants
import rdts.datatypes.experiments.protocols.{Consensus, Participants}
// imports from this file
import Paxos.given
import util.*
import util.Agreement.*

// Paxos PRDT
type LeaderElection = Voting[Uid]
case class PaxosRound[A](
                          leaderElection: LeaderElection =
                          Voting(Set.empty[Vote[Uid]]),
                          proposals: Voting[A] = Voting[A](Set.empty[Vote[A]])
                        )
case class BallotNum(uid: Uid, counter: Long)

case class Paxos[A](
                     rounds: Map[BallotNum, PaxosRound[A]] =
                     Map.empty[BallotNum, PaxosRound[A]]
                   ) {
  // voting
  def voteLeader(leader: Uid)(using
                              LocalUid,
                              Participants
  ): PaxosRound[A] =
    PaxosRound(leaderElection =
      currentRound.getOrElse(PaxosRound()).leaderElection.voteFor(leader)
    )
  def voteValue(value: A)(using
                          LocalUid,
                          Participants
  ): PaxosRound[A] =
    PaxosRound(proposals =
      currentRound.getOrElse(PaxosRound()).proposals.voteFor(value)
    )


  // boolean threshold queries
  def currentRoundHasCandidate: Boolean = currentRound match
    case Some(PaxosRound(leaderElection, _))
      if leaderElection.votes.nonEmpty => true
    case _ => false
  def isCurrentLeader(using
                      Participants,
                      LocalUid
                     ): Boolean = currentRound match
    case Some(PaxosRound(leaderElection, _))
      if leaderElection.decision == Decided(replicaId) =>
      true
    case _ => false
  def currentRoundHasProposal: Boolean  = currentRound match
    case Some(PaxosRound(_, proposals))
      if proposals.votes.nonEmpty => true
    case _ => false

  // protocol actions:
  def phase1a(using LocalUid, Participants): Paxos[A] =
    // try to become leader
    Paxos(Map(nextBallotNum -> voteLeader(replicaId)))
  def phase1a(value: A)(using LocalUid, Participants): Paxos[A] =
    // try to become leader
    Paxos(Map(nextBallotNum -> voteLeader(replicaId), BallotNum(replicaId, -1) -> voteValue(value)))

  def phase1b(using LocalUid, Participants): Paxos[A] =
    updateIf(currentRoundHasCandidate)(
      // vote in the current leader election
      lastValueVote match
        case Some(promisedBallot, acceptedVal) =>
          // vote for candidate and include value most recently voted for
          Paxos(Map(
            currentBallotNum -> voteLeader(leaderCandidate),
            promisedBallot   -> acceptedVal
          )) // previously accepted value
        case None                              =>
          // no value voted for, just vote for candidate
          Paxos(Map(
            currentBallotNum -> voteLeader(leaderCandidate)
          ))
    )

  def phase2a(using LocalUid, Participants): Paxos[A] =
    // propose a value if I am the leader
    updateIf(isCurrentLeader)(
      if newestReceivedVal.nonEmpty then
        // propose most recent received value
        Paxos(Map(currentBallotNum -> voteValue(
          newestReceivedVal.get
        )))
      else
        // no values received during leader election, propose my value
        Paxos(Map(currentBallotNum -> voteValue(myValue)))
    )

  def phase2b(using LocalUid, Participants): Paxos[A] =
    // accept proposed value
    updateIf(currentRoundHasProposal) {
      val proposal =
        currentRound.get.proposals.votes.head.value
      Paxos(Map(currentBallotNum -> voteValue(proposal)))
    }

  // decision function
  def dec(using Participants): Agreement[A] =
    rounds.collectFirst {
      case (b, PaxosRound(_, proposals))
        if proposals.decision != Undecided =>
        proposals.decision
    }.getOrElse(Undecided)

  // helper functions
  def nextBallotNum(using LocalUid): BallotNum          =
    val maxCounter: Long = rounds
      .map((b, _) => b.counter)
      .maxOption
      .getOrElse(-1)
    BallotNum(replicaId, maxCounter + 1)
  def currentRound: Option[PaxosRound[A]]               =
    rounds.maxOption.map(_._2)
  def currentBallotNum: BallotNum                       =
    rounds.maxOption.map(_._1).get
  def leaderCandidate: Uid                              =
    currentLeaderElection.map(_.votes.head.value).get
  def currentLeaderElection: Option[LeaderElection]     =
    currentRound match
      case Some(PaxosRound(leaderElection, _)) =>
        Some(leaderElection)
      case None                                => None
  def lastValueVote: Option[(BallotNum, PaxosRound[A])] =
    rounds.filter(_._2.proposals.votes.nonEmpty).maxOption
  def newestReceivedVal(using LocalUid)                 =
    lastValueVote.map(_._2.proposals.votes.head.value)
  def myValue(using LocalUid): A = rounds(BallotNum(
    replicaId,
    -1
  )).proposals.votes.head.value
}

object Paxos {
  given [A]: Lattice[PaxosRound[A]] = Lattice.derived
  given l[A]: Lattice[Paxos[A]]     = Lattice.derived

  given Ordering[BallotNum] with
    override def compare(x: BallotNum, y: BallotNum): Int =
      if x.counter > y.counter then 1
      else if x.counter < y.counter then -1
      else Ordering[Uid].compare(x.uid, y.uid)
  given [A]: Ordering[(BallotNum, PaxosRound[A])] with
    override def compare(
                          x: (BallotNum, PaxosRound[A]),
                          y: (BallotNum, PaxosRound[A])
                        ): Int = (x, y) match
      case ((x, _), (y, _)) =>
        Ordering[BallotNum].compare(x, y)

  given [A]: Bottom[Paxos[A]] = Bottom.provide(Paxos())

  given Consensus[Paxos] with
    extension [A](c: Paxos[A])
      override def propose(value: A)(using LocalUid, protocols.Participants): Paxos[A] =
        // check if I can propose a value
        if c.isCurrentLeader then
          c.phase2a
        else
          c.phase1a(value)
    extension [A](c: Paxos[A])
      override def decision(using protocols.Participants): Option[A] =
        c.dec match
          case Agreement.Invalid => None
          case Agreement.Decided(value) => Some(value)
          case Agreement.Undecided => None
    extension [A](c: Paxos[A])
      override def upkeep()(using LocalUid, protocols.Participants): Paxos[A] =
        if c.currentRoundHasProposal then
          c.phase2b
        else if c.isCurrentLeader then
          c.phase2a
        else
          c.phase1b

    override def empty[A]: Paxos[A] = Paxos()

    override def lattice[A]: Lattice[Paxos[A]] = l
}

// voting PRDT
case class Vote[A](voter: Uid, value: A)
case class Voting[A](votes: Set[Vote[A]]) {
  // boolean threshold queries
  def hasNotVoted(using LocalUid): Boolean =
    !votes.exists {
      case Vote(r, _) => r == replicaId
    }

  // decision function
  def hasDuplicateVotes: Boolean          =
    votes.groupBy(
      _.voter
    ).values.filter(_.size > 1).nonEmpty
  def getLeadingValue(): Option[(A, Int)] =
    votes.groupBy(_.value).map((value, vts) =>
      (value, vts.size)
    ).maxByOption(_._2)
  def majority(using Participants)        =
    participants.size / 2 + 1

  def decision(using Participants): Agreement[A] =
    if hasDuplicateVotes then Invalid
    else
      getLeadingValue() match
        case Some(value, count) if count >= majority =>
          Decided(value)
        case _ => Undecided

  // protocol actions
  def voteFor(value: A)(using LocalUid): Voting[A] =
    updateIf(hasNotVoted)(
      Voting(Set(Vote(replicaId, value)))
    )
}

object Voting {
  given [A]: Lattice[Voting[A]] = Lattice.derived
  given [A]: Bottom[Voting[A]]  =
    Bottom.provide(Voting(Set.empty[Vote[A]]))
}

// helper definitions to make this file compile
object util:
  enum Agreement[+A]:
    case Invalid
    case Decided(value: A)
    case Undecided

  def updateIf[P: Bottom](condition: Boolean)(update: => P)
  : P =
    if (condition) then
      update
    else
      Bottom[P].empty
