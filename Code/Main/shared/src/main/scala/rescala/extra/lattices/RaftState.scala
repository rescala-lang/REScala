package rescala.extra.lattices

import rescala.extra.lattices.IdUtil.Id
import rescala.extra.lattices.RaftState._

import scala.util.Try


case class RaftState[T](participants: Set[Id] = Set.empty, leaderVotes: Set[Vote] = Set.empty, valueVotes: Set[Propose[T]] = Set.empty[Propose[T]]) {
  val consensusSize: Int = (participants.size + 1) / 2

  val (
    currentTerm: Int,
    maxTerm: Int,
    leader: Id
    ) = {
    val grouped          = leaderVotes.groupBy(v => (v.term, v.leader))
    val (cterm, cleader) = Try {
      grouped.iterator
             .map { case (k, v) => (k, v.size) }
             .filter(_._2 >= consensusSize)
             .map(_._1)
             .max
    }.getOrElse((0, ""))

    val mterm = Try {
      grouped.iterator
             .map(_._1._1)
             .max
    }.getOrElse(0)
    (cterm, mterm, cleader)
  }

  val nextProposal: Int = Try {valueVotes.iterator.map(_.pos).max + 1}.getOrElse(0)

  def compress: RaftState[T] = copy(leaderVotes = leaderVotes.filter(_.term >= currentTerm))

  def supportVoteDelta(me: Id): RaftState[T] = {
    val voted = valueVotes
      .filter(proposal => proposal.term == currentTerm && proposal.voter == leader)
      .map(proposal => proposal.copy(voter = me))
    RaftState[T](valueVotes = voted)
  }


  def proposeDelta(me: Id, value: T): Option[RaftState[T]] = {
    if (me != leader) None
    else Some {
      RaftState[T](valueVotes = Set(Propose(currentTerm, me, nextProposal, value)))
    }
  }

  lazy val values: List[T] = {
    valueVotes
      .groupBy(p => (p.pos, p.value))
      .iterator
      .filter {_._2.size >= consensusSize}
      .map(_._1)
      .toList
      .sortBy(_._1)
      .map(_._2)
  }
}

object RaftState {

  case class Vote(term: Int, leader: Id, voter: Id)
  case class Propose[T](term: Int, voter: Id, pos: Int, value: T)

  implicit def raftLatticeInstance[T]: Lattice[RaftState[T]] = new Lattice[RaftState[T]] {
    override def merge(left: RaftState[T], right: RaftState[T]): RaftState[T] = {
      RaftState(
        Lattice.merge(left.participants, right.participants),
        Lattice.merge(left.leaderVotes, right.leaderVotes),
        Lattice.merge(left.valueVotes, right.valueVotes))
    }
  }


}
