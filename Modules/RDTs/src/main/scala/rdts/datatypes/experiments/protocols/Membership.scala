package rdts.datatypes.experiments.protocols

import rdts.base.LocalUid.replicaId
import rdts.base.{Lattice, LocalUid, Uid}
import rdts.datatypes.experiments.protocols.Consensus.{syntax, given}
import rdts.time.Time

class LogHack(on: Boolean) {
  inline def info(arg: String): Unit = if on then println(arg) else ()
}

case class Membership[A, C[_], D[_]](
    counter: Time,
    membersConsensus: C[Set[Uid]],
    innerConsensus: D[A],
    log: List[A],
    membershipChanging: Boolean = false
)(using
    Consensus[C],
    Consensus[D]
) {
  private def unchanged: Membership[A, C, D] = Membership(
    counter = counter,
    membersConsensus = Consensus[C].empty,
    innerConsensus = Consensus[D].empty,
    log = List()
  )

  override def toString: String =
    s"Membership(counter: $counter, members: $currentMembers,log: $log, membershipChanging: $membershipChanging)".stripMargin

  def currentMembers: Set[Uid] =
    assert(membersConsensus.members == innerConsensus.members, "Membership of both consensus protocols is the same")
    membersConsensus.members

  def addMember(id: Uid)(using LocalUid): Membership[A, C, D] =
    if isMember then
      unchanged.copy(
        membershipChanging = true,
        membersConsensus = membersConsensus.write(currentMembers + id)
      )
    else unchanged

  def removeMember(id: Uid)(using LocalUid): Membership[A, C, D] =
    if currentMembers.size > 1 && isMember then // cannot remove last member
      unchanged.copy(
        membershipChanging = true,
        membersConsensus = membersConsensus.write(currentMembers - id)
      )
    else unchanged

  def read: List[A] = log

  def write(value: A)(using LocalUid): Membership[A, C, D] =
    if !membershipChanging && isMember then
      unchanged.copy(
        innerConsensus = innerConsensus.write(value)
      )
    else unchanged

  def isMember(using LocalUid): Boolean = currentMembers.contains(replicaId)

  def upkeep()(using rid: LocalUid, logger: LogHack): Membership[A, C, D] =
    if !isMember then return unchanged // do nothing if we are not a member anymore
    val newMembers = membersConsensus.merge(membersConsensus.upkeep())
    val newInner   = innerConsensus.merge(innerConsensus.upkeep())
    (newMembers.read, newInner.read) match
      // member consensus reached -> members have changed
      case (Some(members), _) =>
        logger.info(s"Member consensus reached on members $members")
        copy(
          counter = counter + 1,
          membersConsensus = membersConsensus.reset(members),
          innerConsensus = innerConsensus.reset(members),
          membershipChanging = false
        )
      // inner consensus is reached
      case (None, Some(value)) if !membershipChanging =>
        val newLog = log :+ value
        if newLog.length > 1 then
          logger.info(s"Inner consensus reached on value $value, log: $newLog")
        copy(
          counter = counter + 1,
          membersConsensus = membersConsensus.reset(currentMembers),
          innerConsensus = innerConsensus.reset(currentMembers),
          log = newLog
        )
      // nothing has changed
      case _ =>
        unchanged.copy(
          membersConsensus = newMembers,
          innerConsensus = newInner
        )
}

object Membership {

  def init[A, C[_], D[_]](initialMembers: Set[Uid])(using
      Consensus[C],
      Consensus[D]
  ): Membership[A, C, D] =
    require(initialMembers.nonEmpty, "initial members can't be empty")
    Membership(
      0,
      Consensus[C].init[Set[Uid]](initialMembers),
      Consensus[D].init[A](initialMembers),
      List()
    )

  given lattice[A, C[_], D[_]](using
      Consensus[C],
      Consensus[D]
  ): Lattice[Membership[A, C, D]] with
    override def merge(left: Membership[A, C, D], right: Membership[A, C, D]): Membership[A, C, D] =
      if left.counter > right.counter then left
      else if right.counter > left.counter then right
      else
        Membership(
          left.counter,
          Lattice[C[Set[Uid]]].merge(left.membersConsensus, right.membersConsensus),
          Lattice[D[A]].merge(left.innerConsensus, right.innerConsensus),
          left.log,
          left.membershipChanging || right.membershipChanging
        )
}
