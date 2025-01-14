package rdts.datatypes.experiments.protocols

import rdts.base.{Bottom, Lattice, LocalUid, Uid}
import rdts.datatypes.experiments.protocols.Paxos.given
import rdts.datatypes.{Epoch, LastWriterWins}

enum MultipaxosPhase:
  case LeaderElection
  case Voting
  case Idle

case class MultiPaxosRound[A](
    leader: LastWriterWins[Option[Uid]] = LastWriterWins.empty,
    phase: LastWriterWins[MultipaxosPhase] = LastWriterWins.fallback(MultipaxosPhase.LeaderElection),
    paxos: Paxos[A] = Paxos[A]()
)

case class MultiPaxos[A](
    rounds: Epoch[MultiPaxosRound[A]] = Epoch.empty[MultiPaxosRound[A]],
    log: Map[Long, A] = Map.empty
):

  // private helper functions
  private def currentPaxos = rounds.value.paxos

  // public API
  def leader: Option[Uid]    = rounds.value.leader.value
  def phase: MultipaxosPhase = rounds.value.phase.value

  def startLeaderElection(using LocalUid, Participants): MultiPaxos[A] =
    MultiPaxos(
      rounds =
        // transition to new epoch
        rounds.epocheWrite(MultiPaxosRound(
          leader = LastWriterWins.now(None), // set leader to none
          paxos = Paxos().phase1a,           // start new Paxos round with self proposed as leader
          phase = LastWriterWins.now(MultipaxosPhase.LeaderElection)
        ))
    )

  def proposeIfLeader(value: A)(using LocalUid, Participants): MultiPaxos[A] =
    val afterProposal = currentPaxos.phase2a(value)
    // check if proposing does anything, otherwise return empty delta
    if currentPaxos.subsumes(afterProposal) then
      MultiPaxos()
    else
      MultiPaxos(
        rounds = rounds.write(MultiPaxosRound(
          paxos = afterProposal, // phase 2a already checks if I am the leader
          phase = LastWriterWins.now(MultipaxosPhase.Voting)
        ))
      )

  def upkeep(using LocalUid, Participants): MultiPaxos[A] = {
    // perform upkeep in Paxos
    val deltaPaxos = currentPaxos.upkeep()
    val newPaxos   = currentPaxos.merge(deltaPaxos)

    rounds.value.phase.value match
      case MultipaxosPhase.LeaderElection =>
        // we are in a leader election
        // check if there is a result
        newPaxos.currentRound match
          case Some((_, PaxosRound(leaderElection, proposals))) if leaderElection.result.nonEmpty =>
            // there is a result
            MultiPaxos(rounds =
              rounds.write(MultiPaxosRound(
                leader = LastWriterWins.now(leaderElection.result),
                paxos = deltaPaxos,
                phase = LastWriterWins.now(MultipaxosPhase.Idle)
              ))
            )
          case _ =>
            // there is no result, return upkeep result
            MultiPaxos(rounds =
              rounds.write(MultiPaxosRound(
                paxos = deltaPaxos
              ))
            )
      case MultipaxosPhase.Voting =>
        // we are voting on proposals
        // check if there is a decision
        (newPaxos.decision, newPaxos.decidedLeaderElection) match
          case (Some(decision), Some((ballotNum, leaderElection))) =>
            // append log
            val newLog = Map(rounds.counter -> decision)
            // create new Paxos where leader is already elected
            val newPaxos = Paxos(rounds =
              Map(ballotNum -> PaxosRound(
                leaderElection = leaderElection,
                proposals = Voting[A]()
              ))
            )
            // return new Multipaxos with: appended log, set leader, phase as idle,
            MultiPaxos(
              rounds = rounds.epocheWrite(MultiPaxosRound(
                leader = LastWriterWins.now(leader),
                phase = LastWriterWins.now(MultipaxosPhase.Idle),
                paxos = newPaxos
              )),
              log = newLog
            )
          case _ =>
            // no decision yet, just send upkeep result
            MultiPaxos(rounds =
              rounds.write(MultiPaxosRound(
                paxos = deltaPaxos
              ))
            )
      case MultipaxosPhase.Idle =>
        // nothing to do
        MultiPaxos()
  }

object MultiPaxos:
  given [A]: Bottom[MultiPaxosRound[A]]  = Bottom.provide(MultiPaxosRound())
  given [A]: Lattice[MultiPaxosRound[A]] = Lattice.derived

  // for the log
  given [A]: Lattice[Map[Long, A]] =
    given Lattice[A] = Lattice.assertEquals
    Lattice.mapLattice

  given [A]: Lattice[MultiPaxos[A]] = Lattice.derived

//  given consensus: Consensus[MultiPaxos] =
//    extension [A](c: MultiPaxos[A])
//      override def propose
