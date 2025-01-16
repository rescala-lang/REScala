package rdts.datatypes.experiments.protocols

import rdts.base.{Bottom, Lattice, LocalUid, Uid}
import rdts.datatypes.Epoch
import rdts.datatypes.experiments.protocols.Paxos.given

enum MultipaxosPhase:
  case LeaderElection
  case Voting
  case Idle

case class MultiPaxos[A](
    rounds: Epoch[Paxos[A]] = Epoch.empty[Paxos[A]],
    log: Map[Long, A] = Map.empty
):

  // private helper functions
  private def currentPaxos = rounds.value

  // public API
  def leader(using Participants): Option[Uid] = currentPaxos.currentRound match
    case Some((_, PaxosRound(leaderElection, _))) => leaderElection.result
    case None                                     => None

  def phase(using Participants): MultipaxosPhase =
    currentPaxos.currentRound match
      case None                                                                      => MultipaxosPhase.LeaderElection
      case Some((_, PaxosRound(leaderElection, _))) if leaderElection.result.isEmpty => MultipaxosPhase.LeaderElection
      case Some((_, PaxosRound(leaderElection, proposals)))
          if leaderElection.result.nonEmpty && proposals.votes.nonEmpty => MultipaxosPhase.Voting
      case Some((_, PaxosRound(leaderElection, proposals)))
          if leaderElection.result.nonEmpty && proposals.votes.isEmpty => MultipaxosPhase.Idle
      case _ => throw new Error("Inconsistent Paxos State")

  def read: List[A] = log.toList.sortBy(_._1).map(_._2)

  def startLeaderElection(using LocalUid, Participants): MultiPaxos[A] =
    MultiPaxos(rounds.write(currentPaxos.phase1a)) // start new Paxos round with self proposed as leader

  def proposeIfLeader(value: A)(using LocalUid, Participants): MultiPaxos[A] =
    val afterProposal = currentPaxos.phase2a(value)
    // check if proposing does anything, otherwise return empty delta
    if currentPaxos.subsumes(afterProposal) then
      MultiPaxos()
    else
      MultiPaxos(rounds = rounds.write(afterProposal)) // phase 2a already checks if I am the leader

  def upkeep(using LocalUid, Participants): MultiPaxos[A] = {
    // perform upkeep in Paxos
    val deltaPaxos = currentPaxos.upkeep()
    val newPaxos   = currentPaxos.merge(deltaPaxos)

    (newPaxos.decision, newPaxos.newestBallotWithLeader) match
      case (Some(decision), Some((ballotNum, PaxosRound(leaderElection, _)))) =>
        // we are voting on proposals and there is a decision

        val newLog = Map(rounds.counter -> decision) // append log
        // create new Paxos where leader is already elected
        val newPaxos = Paxos(rounds =
          Map(ballotNum -> PaxosRound(
            leaderElection = leaderElection,
            proposals = Voting[A]()
          ))
        )
        // return new Multipaxos with: appended log
        MultiPaxos(
          rounds = rounds.epocheWrite(newPaxos),
          log = newLog
        )
      case _ =>
        // nothing to do, return upkeep result
        MultiPaxos(rounds = rounds.write(deltaPaxos))
  }

  override def toString: String =
    lazy val s = s"MultiPaxos(epoch: ${rounds.counter}, log: $read)"
    s

object MultiPaxos:
  // for the log
  given [A]: Lattice[Map[Long, A]] =
    given Lattice[A] = Lattice.assertEquals
    Lattice.mapLattice

  given [A]: Lattice[MultiPaxos[A]] = Lattice.derived
