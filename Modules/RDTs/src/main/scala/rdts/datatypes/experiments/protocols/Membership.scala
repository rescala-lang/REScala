package rdts.datatypes.experiments.protocols

import rdts.base.{Bottom, Lattice, LocalUid, Uid}
import rdts.time.Time
import rdts.datatypes.GrowOnlyList
import rdts.datatypes.experiments.protocols.Consensus

case class Membership[A, C[_], D[_]]
(counter: Time,
 membersConsensus: C[Set[Uid]],
 innerConsensus: D[A],
 log: List[A])
(using
 Bottom[C[Set[Uid]]],
 Bottom[D[A]],
 Consensus[C],
 Consensus[D]
) {
  def currentMembers: Set[Uid] = membersConsensus.members

  def addMember(id: Uid)(using LocalUid): Membership[A, C, D] =
    Membership.empty[A, C, D].copy(counter = counter, membersConsensus = membersConsensus.write(currentMembers + id))

  def removeMember(id: Uid)(using LocalUid): Membership[A, C, D] =
    Membership.empty[A, C, D].copy(counter = counter, membersConsensus = membersConsensus.write(currentMembers - id))

  def read: List[A] = log

  def write(value: A)(using LocalUid): Membership[A, C, D] =
    Membership.empty[A, C, D].copy(
      counter = counter,
      innerConsensus = innerConsensus.write(value))

  def upkeep()(using LocalUid): Membership[A, C, D] =
    val newMembers = membersConsensus.upkeep()
    val newInner = innerConsensus.upkeep()
    (newMembers.read, newInner.read) match
      // member consensus reached -> members have changed
      case (Some(members), _) =>
        //        println("Member consensus reached")
        Membership.empty[A, C, D].copy(
          counter = counter + 1,
          membersConsensus = membersConsensus.reset(members),
          innerConsensus = innerConsensus.reset(members)
        )
      // inner consensus is reached
      case (None, Some(value)) =>
        //        println(s"Inner consensus reached on value $value")
        Membership.empty[A, C, D].copy(
          counter = counter + 1,
          membersConsensus = membersConsensus.reset(currentMembers),
          innerConsensus = innerConsensus.reset(currentMembers),
          log = log :+ value
        )
      // nothing has changed
      case (None, None) =>
        Membership.empty[A, C, D].copy(
          counter = counter,
          membersConsensus = newMembers,
          innerConsensus = newInner
        )
}

object Membership {
  def init[A, C[_], D[_]]
  (initialMembers: Set[Uid])
  (using
   Bottom[C[Set[Uid]]],
   Bottom[D[A]],
   Consensus[C],
   Consensus[D]
  ): Membership[A, C, D] =
    val unchanged = Membership.empty[A, C, D]
    unchanged.copy(membersConsensus = unchanged.membersConsensus.reset(initialMembers),
      innerConsensus = unchanged.innerConsensus.reset(initialMembers))


  def empty[A, C[_], D[_]]
  (using
   Bottom[C[Set[Uid]]],
   Bottom[D[A]],
   Consensus[C],
   Consensus[D]
  ): Membership[A, C, D] =
    Membership(
      0,
      Bottom[C[Set[Uid]]].empty,
      Bottom[D[A]].empty,
      List()
    )

  given lattice[A, C[_], D[_]]
  (using
   Bottom[C[Set[Uid]]],
   Bottom[D[A]],
   Consensus[C],
   Consensus[D],
   Lattice[C[Set[Uid]]],
   Lattice[D[A]],
  ): Lattice[Membership[A, C, D]] with
    override def merge(left: Membership[A, C, D], right: Membership[A, C, D]): Membership[A, C, D] =
      if left.counter > right.counter then left
      else if right.counter > left.counter then right
      else
        Membership(
          left.counter,
          Lattice[C[Set[Uid]]].merge(left.membersConsensus, right.membersConsensus),
          Lattice[D[A]].merge(left.innerConsensus, right.innerConsensus),
          left.log
        )
}
