package rdts.datatypes.experiments.protocols.simplified

import rdts.base.LocalUid.replicaId
import rdts.base.{LocalUid, Uid}
import rdts.datatypes.experiments.protocols.Participants
import rdts.datatypes.experiments.protocols.Participants.participants
import rdts.datatypes.experiments.protocols.simplified.PrepareAbort.{Abort, Prepare}

enum PrepareAbort:
  case Prepare
  case Abort

case class Acknowledge()

case class TwoPhaseCommit[A](
    transaction: Option[A] = None,
    voting: SimpleVoting[PrepareAbort] = SimpleVoting(),
    acknowledgement: Set[Uid] = Set.empty
):

  // as the coordinator, propose a transaction
  def commitRequest(transaction: A)(using LocalUid, Participants): TwoPhaseCommit[A] =
    // check if there is a transaction ongoing
    transaction match
      case None => TwoPhaseCommit(Some(transaction), voting = voting.voteFor(Prepare))
      case Some => TwoPhaseCommit()

  // as a participant, vote for commit in the request phase
  def prepare(using LocalUid, Participants): TwoPhaseCommit[A] =
    transaction match
      case Some(_) => TwoPhaseCommit(voting = voting.voteFor(Prepare))
      case None    => TwoPhaseCommit()

  // as a participant, vote for abort in the request phase
  def abort(using LocalUid, Participants): TwoPhaseCommit[A] =
    transaction match
      case Some(_) => TwoPhaseCommit(voting = voting.voteFor(Abort))
      case None    => TwoPhaseCommit()

  // as a participant,
  // check if request phase was accepted by everyone
  // commit the transaction and send ack to the others
  def acknowledge(using LocalUid, Participants): TwoPhaseCommit[A] =
    // check if there is a transaction and everybody has voted
    if transaction.isDefined && voting.votes.size == participants.size
//      voting.votes.count(_.value == Prepare) == participants.size
    then
      TwoPhaseCommit(acknowledgement = Set(replicaId))
    else TwoPhaseCommit()
