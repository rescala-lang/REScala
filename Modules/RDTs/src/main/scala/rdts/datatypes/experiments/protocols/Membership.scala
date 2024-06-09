package rdts.datatypes.experiments.protocols

import rdts.base.{Bottom, Lattice, Orderings, Uid}
import rdts.time.Time

trait Consensus[C[_]] {
  extension [A](c: C[A]) def write(value: A): C[A]
  extension [A](c: C[A]) def read: Option[A]
  extension [A](c: C[A]) def members: Set[Uid]
  extension [A](c: C[A]) def reset(newMembers: Set[Uid]): C[A]
  extension [A](c: C[A]) def upkeep: C[A]
}

//trait Consensus[C[A]] {
//  def write[A](value: A): C[A]
//  def read[A]: A
//  def members: Set[Uid]
//}

case class Membership[A, C[_]: Consensus, D[_]: Consensus](
    counter: Time,
    membersConsensus: C[Set[Uid]],
    innerConsensus: C[A],
    log: List[A]
) {
  def currentMembers: Set[Uid] = membersConsensus.members
  def addMember(id: Uid): Membership[A, C, D] =
    Membership.unchanged.copy(membersConsensus = membersConsensus.write(currentMembers + id))
  def removeMember(id: Uid): Membership[A, C, D] =
    Membership.unchanged.copy(membersConsensus = membersConsensus.write(currentMembers - id))
  def read            = log
  def write(value: A) = innerConsensus.write(value)

  def upkeep =
    val newMembers = membersConsensus.upkeep
    val newInner   = innerConsensus.upkeep
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

  given lattice[A, C[_], D[_]]: Lattice[Membership[A, C, D]] with
    override def merge(left: Membership[A, C, D], right: Membership[A, C, D]): Membership[A, C, D] =
      val counter =
        if left.counter < right.counter then right.counter
        else left.counter

      ???
}
