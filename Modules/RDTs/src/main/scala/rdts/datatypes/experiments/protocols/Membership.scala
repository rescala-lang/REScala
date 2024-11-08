package rdts.datatypes.experiments.protocols

import rdts.base.LocalUid.replicaId
import rdts.base.{Lattice, LocalUid, Uid}
import rdts.datatypes.experiments.protocols.Consensus.given
import rdts.time.Time

import scala.collection.immutable.NumericRange

class LogHack(on: Boolean) {
  inline def info(arg: => String): Unit = if on then println(arg) else ()
}

case class Membership[A, C[_], D[_]](
    counter: Time,
    membersConsensus: C[Set[Uid]],
    innerConsensus: D[A],
    log: Map[Long, A],
    membershipChanging: Boolean = false,
    members: Set[Uid]
) {
  private def unchanged(using Consensus[C], Consensus[D]): Membership[A, C, D] = Membership(
    counter = counter,
    membersConsensus = Consensus[C].empty,
    innerConsensus = Consensus[D].empty,
    log = Map.empty,
    members = Set.empty
  )

  given Participants = Participants(members)

  override def toString: String =
    s"$Membership(counter: $counter, members: $membersConsensus,log: $log, membershipChanging: $membershipChanging)".stripMargin

  def currentMembers(using Consensus[C], Consensus[D]): Set[Uid] =
    members

  def addMember(id: Uid)(using LocalUid, Consensus[C], Consensus[D]): Membership[A, C, D] =
    if isMember then
      unchanged.copy(
        membershipChanging = true,
        membersConsensus = membersConsensus.write(currentMembers + id)
      )
    else unchanged

  def removeMember(id: Uid)(using LocalUid, Consensus[C], Consensus[D]): Membership[A, C, D] =
    if currentMembers.size > 1 && isMember then // cannot remove last member
      unchanged.copy(
        membershipChanging = true,
        membersConsensus = membersConsensus.write(currentMembers - id)
      )
    else unchanged

  def read: List[A] = log.toList.sortBy(_._1).map(_._2)

  def readDecisionsSince(time: Time): Iterable[A] =
    NumericRange(time, counter, 1L).view.flatMap(log.get)

  def write(value: A)(using LocalUid, Consensus[C], Consensus[D]): Membership[A, C, D] =
    if !membershipChanging && isMember then
      unchanged.copy(
        innerConsensus = innerConsensus.write(value)
      )
    else unchanged

  def isMember(using LocalUid, Consensus[C], Consensus[D]): Boolean = currentMembers.contains(replicaId)

  def upkeep()(using rid: LocalUid, logger: LogHack, cc: Consensus[C], cd: Consensus[D]): Membership[A, C, D] =
    if !isMember then return unchanged // do nothing if we are not a member anymore
    val newMembers = membersConsensus.merge(membersConsensus.upkeep())
    val newInner   = innerConsensus.merge(innerConsensus.upkeep())
    (newMembers.read, newInner.read) match
      // member consensus reached -> members have changed
      case (Some(members), _) =>
        logger.info { s"Member consensus reached on members $members" }
        copy(
          counter = counter + 1,
          membersConsensus = Consensus[C].empty,
          innerConsensus = Consensus[D].empty,
          membershipChanging = false,
          members = members
        )
      // inner consensus is reached
      case (None, Some(value)) if !membershipChanging =>
        val newLog = Map(counter -> value)
        logger.info { s"$rid: Inner consensus reached on value $value, log: $newLog" }
        copy(
          counter = counter + 1,
          membersConsensus = Consensus[C].empty,
          innerConsensus = Consensus[D].empty,
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
      counter = 0,
      membersConsensus = Consensus[C].empty,
      innerConsensus = Consensus[D].empty,
      log = Map.empty,
      members = initialMembers
    )

  def logLattice[A]: Lattice[Map[Long, A]] = Lattice.mapLattice(using Lattice.assertEquals)

  given lattice[A, C[_], D[_]](using
      Consensus[C],
      Consensus[D]
  ): Lattice[Membership[A, C, D]] with
    override def merge(left: Membership[A, C, D], right: Membership[A, C, D]): Membership[A, C, D] =
      if left.counter > right.counter then left.copy(log = logLattice.merge(left.log, right.log))
      else if right.counter > left.counter then right.copy(log = logLattice.merge(right.log, left.log))
      else
        Membership(
          left.counter,
          Lattice[C[Set[Uid]]].merge(left.membersConsensus, right.membersConsensus),
          Lattice[D[A]].merge(left.innerConsensus, right.innerConsensus),
          logLattice.merge(left.log, right.log),
          left.membershipChanging || right.membershipChanging,
          left.members
        )

    override def lteq(left: Membership[A, C, D], right: Membership[A, C, D]): Boolean =
      if left.counter < right.counter then true
      else
        Lattice[D[A]].lteq(
          left.innerConsensus,
          right.innerConsensus
        ) && Lattice[C[Set[Uid]]].lteq(left.membersConsensus, right.membersConsensus)
}
