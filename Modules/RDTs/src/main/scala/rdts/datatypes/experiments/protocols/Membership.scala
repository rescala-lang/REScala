package rdts.datatypes.experiments.protocols

import rdts.base.LocalUid.replicaId
import rdts.base.{Bottom, Lattice, LocalUid, Uid}
import rdts.time.Time

case class Membership[A, C[_], D[_]]
(counter: Time,
 membersConsensus: C[Set[Uid]],
 innerConsensus: D[A],
 log: List[A],
 membershipChanging: Boolean = false)
(using
 Bottom[C[Set[Uid]]],
 Bottom[D[A]],
 Consensus[C],
 Consensus[D],
 Lattice[C[Set[Uid]]],
 Lattice[D[A]]
) {

  override def toString: String =
    s"Membership(counter: $counter, members: ${currentMembers},log: $log, membershipChanging: $membershipChanging)".stripMargin

  def currentMembers: Set[Uid] =
    assert(membersConsensus.members == innerConsensus.members, "Membership of both consensus protocols is the same")
    membersConsensus.members

  def addMember(id: Uid)(using LocalUid): Membership[A, C, D] =
    if isMember then
      copy(
        membershipChanging = true,
        membersConsensus = membersConsensus.merge(membersConsensus.write(currentMembers + id)))
    else this

  def removeMember(id: Uid)(using LocalUid): Membership[A, C, D] =
    if currentMembers.size > 1 && isMember then // cannot remove last member
      copy(
        membershipChanging = true,
        membersConsensus = membersConsensus.merge(membersConsensus.write(currentMembers - id)))
    else this

  def read: List[A] = log

  def write(value: A)(using LocalUid): Membership[A, C, D] =
    if !membershipChanging && isMember then
      copy(
        innerConsensus = innerConsensus.merge(innerConsensus.write(value)))
    else this

  def isMember(using LocalUid) = currentMembers.contains(replicaId)

  def upkeep()(using LocalUid): Membership[A, C, D] =
    if !isMember then return this // do nothing if we are not a member anymore
    val memberUpkeep = membersConsensus.upkeep()
    val innerUpkeep = innerConsensus.upkeep()
    val newMembers = membersConsensus.merge(membersConsensus.upkeep())
    val newInner = innerConsensus.merge(innerConsensus.upkeep())
    (newMembers.read, newInner.read) match
      // member consensus reached -> members have changed
      case (Some(members), _) =>
        println(s"Member consensus reached on members $members")
        copy(
          counter = counter + 1,
          membersConsensus = membersConsensus.reset(members),
          innerConsensus = innerConsensus.reset(members),
          membershipChanging = false
        )
      // inner consensus is reached
      case (None, Some(value)) if !membershipChanging =>
        println(s"Inner consensus reached on value $value, log: ${log :+ value}")
        copy(
          counter = counter + 1,
          membersConsensus = membersConsensus.reset(currentMembers),
          innerConsensus = innerConsensus.reset(currentMembers),
          log = log :+ value
        )
      // nothing has changed
      case _ =>
        copy(
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
   Consensus[D],
   Lattice[C[Set[Uid]]],
   Lattice[D[A]],
  ): Membership[A, C, D] =
    require(initialMembers.nonEmpty, "initial members can't be empty")
    val unchanged = Membership.empty[A, C, D]
    unchanged.copy(membersConsensus = unchanged.membersConsensus.reset(initialMembers),
      innerConsensus = unchanged.innerConsensus.reset(initialMembers))


  def empty[A, C[_], D[_]]
  (using
   Bottom[C[Set[Uid]]],
   Bottom[D[A]],
   Consensus[C],
   Consensus[D],
   Lattice[C[Set[Uid]]],
   Lattice[D[A]],
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
        require(left.currentMembers == right.currentMembers, s"left and right members need to be the same. Got: $left, $right")
        Membership(
          left.counter,
          Lattice[C[Set[Uid]]].merge(left.membersConsensus, right.membersConsensus),
          Lattice[D[A]].merge(left.innerConsensus, right.innerConsensus),
          left.log,
          left.membershipChanging || right.membershipChanging
        )
}
