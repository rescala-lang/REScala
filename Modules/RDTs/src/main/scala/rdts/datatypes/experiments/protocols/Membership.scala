package rdts.datatypes.experiments.protocols

import rdts.base.Lattice.syntax
import rdts.base.LocalUid.replicaId
import rdts.base.{Bottom, Lattice, LocalUid, Uid}
import rdts.datatypes.Epoch
import rdts.datatypes.experiments.protocols.Consensus.given
import rdts.time.Time

import scala.collection.immutable.NumericRange

case class MembershipRound[A, C[_], D[_]](
    membersConsensus: C[Set[Uid]],
    innerConsensus: D[A],
    membershipChanging: Boolean = false,
    members: Set[Uid]
)

object MembershipRound {
  given bottom[A, C[_], D[_]](using Consensus[C], Consensus[D]): Bottom[MembershipRound[A, C, D]] = Bottom.provide(
    MembershipRound(
      membersConsensus = Consensus[C].empty,
      innerConsensus = Consensus[D].empty,
      members = Set.empty
    )
  )

  given lattice[A, C[_], D[_]](using Lattice[C[Set[Uid]]], Lattice[D[A]]): Lattice[MembershipRound[A, C, D]] =
    given Lattice[Boolean] = _ || _
    Lattice.derived
}

case class Membership[A, C[_], D[_]](
    rounds: Epoch[MembershipRound[A, C, D]],
    log: Map[Long, A] = Map.empty,
) {

  //TODO: investigate further …
  override def toString: String = "MEMBERSHIP TO STRING MAKES SPEC TESTS SLOW, WTF"

  private def bottomRound(using Consensus[C], Consensus[D]): MembershipRound[A, C, D] =
    MembershipRound.bottom[A, C, D].empty

  given Participants = Participants(rounds.value.members)

  def currentMembers(using Consensus[C], Consensus[D]): Set[Uid] =
    rounds.value.members

  def counter: Time                 = rounds.counter
  def innerConsensus: D[A]          = rounds.value.innerConsensus
  def membersConsensus: C[Set[Uid]] = rounds.value.membersConsensus

  def writeRound(membershipRound: MembershipRound[A, C, D]): Membership[A, C, D] = {
    Membership(rounds.write(membershipRound))
  }

  def addMember(id: Uid)(using LocalUid, Consensus[C], Consensus[D]): Membership[A, C, D] = writeRound {
    if isMember then
      bottomRound.copy(
        membershipChanging = true,
        membersConsensus = rounds.value.membersConsensus.propose(currentMembers + id)
      )
    else bottomRound
  }

  def removeMember(id: Uid)(using LocalUid, Consensus[C], Consensus[D]): Membership[A, C, D] = writeRound {
    if currentMembers.size > 1 && isMember then // cannot remove last member
      bottomRound.copy(
        membershipChanging = true,
        membersConsensus = rounds.value.membersConsensus.propose(currentMembers - id)
      )
    else bottomRound
  }

  def read: List[A] = log.toList.sortBy(_._1).map(_._2)

  def readDecisionsSince(time: Time): Iterable[A] =
    NumericRange(time, rounds.counter, 1L).view.flatMap(log.get)

  def write(value: A)(using LocalUid, Consensus[C], Consensus[D]): Membership[A, C, D] = writeRound {
    if !rounds.value.membershipChanging && isMember then
      bottomRound.copy(
        innerConsensus = rounds.value.innerConsensus.propose(value)
      )
    else bottomRound
  }

  def isMember(using LocalUid, Consensus[C], Consensus[D]): Boolean = currentMembers.contains(replicaId)

  def upkeep()(using rid: LocalUid, cc: Consensus[C], cd: Consensus[D]): Membership[A, C, D] =
    if !isMember then return writeRound(bottomRound) // do nothing if we are not a member anymore
    val deltaMembers = rounds.value.membersConsensus.upkeep()
    val newMembers   = rounds.value.membersConsensus.merge(deltaMembers)
    val deltaInner   = rounds.value.innerConsensus.upkeep()
    val newInner     = rounds.value.innerConsensus.merge(deltaInner)
    (newMembers.decision, newInner.decision) match
      // member consensus reached -> members have changed
      case (Some(members), _) =>
        assert(!members.isEmpty, "members consensus reached but no members found")
        Membership(rounds.epocheWrite(
          MembershipRound(
            membersConsensus = Consensus[C].empty,
            innerConsensus = Consensus[D].empty,
            membershipChanging = false,
            members = members
          )
        ))
      // inner consensus is reached
      case (None, Some(value)) if !rounds.value.membershipChanging =>
        val newLog = Map(rounds.counter -> value)
        Membership(
          rounds.epocheWrite(
            MembershipRound(
              membersConsensus = Consensus[C].empty,
              innerConsensus = Consensus[D].empty,
              membershipChanging = false,
              members = currentMembers
            )
          ),
          log = newLog
        )
      // nothing has changed
      case _ =>
        writeRound(
          bottomRound.copy(
            membersConsensus = deltaMembers,
            innerConsensus = deltaInner
          )
        )
}

object Membership {

  def init[A, C[_], D[_]](initialMembers: Set[Uid])(using
      Consensus[C],
      Consensus[D]
  ): Membership[A, C, D] =
    require(initialMembers.nonEmpty, "initial members can't be empty")
    Membership(
      Epoch(0, MembershipRound.bottom[A, C, D].empty.copy(members = initialMembers)),
      Map.empty
    )

  given latticeFromConsensus[A, C[_], D[_]](using
      ccon: Consensus[C],
      dcon: Consensus[D]
  ): Lattice[Membership[A, C, D]] =
    lattice(using ccon.lattice, dcon.lattice)

  def lattice[A, C[_], D[_]](using Lattice[C[Set[Uid]]], Lattice[D[A]]): Lattice[Membership[A, C, D]] =
    // for the log
    given Lattice[Map[Long, A]] =
      given Lattice[A] = Lattice.assertEquals
      Lattice.mapLattice
    Lattice.derived
}
