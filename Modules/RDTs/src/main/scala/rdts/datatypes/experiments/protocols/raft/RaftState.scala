package rdts.datatypes.experiments.protocols.raft

import rdts.base.{Lattice, Uid}
import rdts.datatypes.experiments.protocols.raft.RaftState.{DecisionImpossible, Propose, Undecided, Vote}

import scala.util.Try

case class RaftState[T](
    participants: Set[Uid] = Set.empty,
    leaderVotes: Set[Vote] = Set.empty,
    valueProposals: Set[Propose[T]] = Set.empty[Propose[T]]
) {
  val consensusSize: Int = (participants.size + 2) / 2

  val (
    currentTerm: Int,
    maxTerm: Int,
    leader: Uid
  ) = {
    val grouped = leaderVotes.groupBy(v => (v.term, v.leader))
    val (cterm, cleader) = Try {
      grouped.iterator
        .map { case (k, v) => (k, v.size) }
        .filter(_._2 >= consensusSize)
        .map(_._1)
        .max
    }.getOrElse((0, Uid.zero))

    val mterm = Try {
      grouped.iterator
        .map(_._1._1)
        .max
    }.getOrElse(0)
    (cterm, mterm, cleader)
  }

  val nextProposal: Int = Try { valueProposals.iterator.map(_.pos).max + 1 }.getOrElse(0)

  def compress: RaftState[T] = copy(leaderVotes = leaderVotes.filter(_.term >= currentTerm))

  def supportProposalDelta(me: Uid): RaftState[T] = {
    val voted = valueProposals
      .filter(proposal => proposal.term == currentTerm && proposal.voter == leader)
      .map(proposal => proposal.copy(voter = me))
    RaftState[T](valueProposals = voted)
  }

  def supportProposal(me: Uid): RaftState[T] = Lattice.merge(this, supportProposalDelta(me))

  def proposeDelta(me: Uid, value: T): RaftState[T] = {
    if me != leader then RaftState[T]()
    else {
      RaftState[T](valueProposals = Set(Propose(currentTerm, me, nextProposal, value)))
    }
  }

  def propose(me: Uid, value: T): RaftState[T] =
    Lattice.merge(this, proposeDelta(me, value))

  def becomeCandidateDelta(me: Uid): RaftState[T] =
    RaftState[T](leaderVotes = Set(Vote(maxTerm + 1, me, me)))

  def becomeCandidate(me: Uid): RaftState[T] =
    Lattice.merge(this, becomeCandidateDelta(me))

  def supportLeaderDelta(me: Uid): RaftState[T] = {
    val votes = leaderVotes.filter(candidate => candidate.term == maxTerm)
    if votes.exists(_.voter == me) then RaftState[T]()
    else {
      if votes.isEmpty then RaftState[T]()
      else {
        val bestCandidate = votes.groupBy(_.leader).maxBy(_._2.size)._1
        RaftState[T](leaderVotes = Set(Vote(maxTerm, bestCandidate, me)))
      }
    }
  }

  def supportLeader(me: Uid): RaftState[T] =
    Lattice.merge(this, supportLeaderDelta(me))

  lazy val byRound: IndexedSeq[Set[Propose[T]]] = {
    val grouped = valueProposals.groupBy(_.pos)
    Range(0, nextProposal).map { pos => grouped.getOrElse(pos, Set.empty) }
  }

  // TODO: this still has an issue when votes for old terms come in
  lazy val values: List[T] = {
    def decision(proposals: Set[Propose[T]]): Option[Option[T]] = {
      val (size, value) =
        proposals.groupBy(_.value).map(g => (g._2.size, Some(g._1))).maxByOption(_._1).getOrElse((0, None))
      if size >= consensusSize then Some(value)
      else {
        val term = proposals.headOption.fold(-1)(_.term)
        if term != currentTerm then DecisionImpossible
        else {
          val undecided = participants.size - proposals.size
          if undecided + size >= consensusSize then Undecided
          else {
            DecisionImpossible
          }
        }
      }
    }
    valueProposals
      .groupBy(p => p.pos)
      .iterator
      .map(g => (g._1, decision(g._2)))
      .toList.sortBy(_._1)
      .iterator.map(_._2)
      .takeWhile(_.isDefined)
      .flatten.flatten.toList

  }
}

object RaftState {

  private val DecisionImpossible = Some(None)
  private val Undecided          = None

  case class Vote(term: Int, leader: Uid, voter: Uid)
  case class Propose[T](term: Int, voter: Uid, pos: Int, value: T)

  given raftLatticeInstance[T]: Lattice[RaftState[T]] = Lattice.derived
}
