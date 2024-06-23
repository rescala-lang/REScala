package rdts.datatypes.experiments.protocols

import rdts.base.{Lattice, LocalUid, Uid}
import rdts.time.Time

case class Membership[A, C[_]: Consensus, D[_]: Consensus](
    counter: Time,
    membersConsensus: C[Set[Uid]],
    innerConsensus: D[A],
    log: List[A]
) {
  def currentMembers: Set[Uid] = membersConsensus.members
  def addMember(id: Uid)(using LocalUid): Membership[A, C, D] =
    Membership.unchanged.copy(membersConsensus = membersConsensus.write(currentMembers + id))
  def removeMember(id: Uid)(using LocalUid): Membership[A, C, D] =
    Membership.unchanged.copy(membersConsensus = membersConsensus.write(currentMembers - id))
  def read: List[A]                         = log
  def write(value: A)(using LocalUid): D[A] = innerConsensus.write(value)

  def upkeep()(using LocalUid): Membership[A, C, D] =
    val newMembers = membersConsensus.upkeep()
    val newInner   = innerConsensus.upkeep()
    (newMembers.read, newInner.read) match
      // members have changed
      case (Some(members), _) =>
        Membership.unchanged.copy[A, C, D](
          counter = counter + 1,
          membersConsensus = membersConsensus.reset(members),
          innerConsensus = innerConsensus.reset(members)
        )
      // inner consensus is reached
      case (None, Some(value)) =>
        Membership.unchanged.copy[A, C, D](
          counter = counter + 1,
          membersConsensus = membersConsensus.reset(currentMembers),
          innerConsensus = innerConsensus.reset(currentMembers),
          log = log :+ value
        )
      // nothing has changed
      case (None, None) =>
        Membership.unchanged.copy[A, C, D](
          membersConsensus = newMembers,
          innerConsensus = newInner
        )
}

object Membership {
  def unchanged[A, C[_], D[_]]: Membership[A, C, D] = ???

  given lattice[A, C[_], D[_]](using
      Consensus[C],
      Consensus[D],
      Lattice[C[Set[Uid]]],
      Lattice[D[A]]
  ): Lattice[Membership[A, C, D]] with
    override def merge(left: Membership[A, C, D], right: Membership[A, C, D]): Membership[A, C, D] =
      if left.counter > right.counter then left
      else if right.counter > left.counter then right
      else
        assert(left.log == right.log) // safety check: logs should never go out of sync without counter increasing
        Membership(
          left.counter,
          Lattice[C[Set[Uid]]].merge(left.membersConsensus, right.membersConsensus),
          Lattice[D[A]].merge(left.innerConsensus, right.innerConsensus),
          left.log
        )
}
