package rdts.datatypes.experiments.protocols

import rdts.base.Lattice.setLattice
import rdts.base.{Bottom, Lattice, Orderings, Uid}
import rdts.datatypes.GrowOnlyMap.*
import rdts.datatypes.GrowOnlySet.*
import rdts.datatypes.{GrowOnlyMap, GrowOnlySet, LastWriterWins}
import rdts.dotted.Dotted
import rdts.syntax.LocalUid
import rdts.syntax.LocalUid.replicaId
import rdts.time.Dots

import scala.compiletime.{constValue, summonFrom}

enum Phase:
  case One
  case Two

// message types
case class Prepare(proposalNumber: Int, proposer: Uid)
case class Promise[A](proposal: Prepare, value: Option[A], acceptor: Uid)
case class Accept[A](proposal: Prepare, value: A)
case class Accepted[A](proposal: Prepare, value: A, acceptor: Uid)

given Ordering[Prepare] with
  override def compare(x: Prepare, y: Prepare): Int =
    if x.proposalNumber > y.proposalNumber then 1
    else if x.proposalNumber == y.proposalNumber then Ordering[Uid].compare(x.proposer, y.proposer)
    else -1

given Ordering[Accept[?]] with
  override def compare(x: Accept[?], y: Accept[?]): Int = Ordering[Prepare].compare(x.proposal, y.proposal)

case class Paxos[A](
    prepares: GrowOnlySet[Prepare],
    promises: GrowOnlySet[Promise[A]],
    accepts: GrowOnlySet[Accept[A]],
    accepteds: GrowOnlySet[Accepted[A]],
    members: Set[Uid] // constant
) {
  private def quorum: Int = members.size / 2 + 1

  def prepare()(using LocalUid): Paxos[A] =
    val proposalNumber = highestProposal.map(_.proposalNumber).getOrElse(-1) + 1
    Paxos.unchanged.copy(
      prepares = prepares.insert(Prepare(proposalNumber, replicaId))
    )

  def promise()(using LocalUid): Paxos[A] =
    val myHighestPromiseNumber =
      promises.filter(_.acceptor == replicaId).map(_.proposal.proposalNumber).maxOption.getOrElse(-1)
    // check if I already promised for an equally high id
    if myHighestPromiseNumber >= highestProposal.map(_.proposalNumber).getOrElse(-1)
    then
      // already promised for equally high id
      Paxos.unchanged
    else
      // there is a new higher proposal
      // check if I already accepted a specific value
      val value =
        accepteds.filter(_.acceptor == replicaId).map(_.value).headOption
      Paxos.unchanged.copy(
        promises = promises.insert(Promise(highestProposal.get, value, replicaId))
      )

  def accept(v: A)(using LocalUid): Paxos[A] =
    val promisesForProposal = myHighestProposal.map(p => promises.filter(_.proposal == p)).getOrElse(Set())
    // check if accepted
    if !canSendAccept then
      // is not accepted
      Paxos.unchanged
    else
      // is accepted, check if promise contains value
      val promisesWithVal = promisesForProposal.filter(_.value.isDefined)
      val value: A        = promisesWithVal.map(_.value).headOption.flatten.getOrElse(v)
      Paxos.unchanged.copy(
        accepts = accepts.insert(Accept(myHighestProposal.get, value))
      )

  def accepted()(using LocalUid): Paxos[A] =
    if newestAccept.isEmpty || // there are no accepts
      // I have already promised a higher proposalNumber
      promises.filter(_.acceptor == replicaId).map(_.proposal.proposalNumber).maxOption.getOrElse(-1) >
      newestAccept.get.proposal.proposalNumber
    then
      Paxos.unchanged
    else
      Paxos.unchanged.copy(accepteds =
        accepteds.insert(Accepted(
          proposal = newestAccept.get.proposal,
          value = newestAccept.get.value,
          acceptor = replicaId
        ))
      )

  def upkeep()(using LocalUid): Paxos[A] =
    // check which phase we are in
    phase match
      case Phase.One if newestPrepare.isDefined => promise()
      case Phase.Two                            => accepted()
      case _                                    => Paxos.unchanged

  // helper functions
  private def newestAccept: Option[Accept[A]] = accepts.maxOption
  private def newestPrepare: Option[Prepare]  = prepares.maxOption
  private def highestProposal: Option[Prepare] =
    prepares.maxOption
  private def myHighestProposal(using LocalUid): Option[Prepare] = prepares.filter(_.proposer == replicaId).maxOption
  private def canSendAccept(using LocalUid): Boolean =
    val promisesForProposal = myHighestProposal.map(p => promises.filter(_.proposal == p)).getOrElse(Set())
    promisesForProposal.size >= quorum
  private def phase: Phase =
    if newestAccept.map(_.proposal.proposalNumber).getOrElse(-1) >= newestPrepare.map(_.proposalNumber).getOrElse(-1)
    then
      Phase.Two
    else
      Phase.One

  // API
  def write(value: A)(using LocalUid): Paxos[A] = (phase, read, myHighestProposal, highestProposal) match
    case (Phase.Two, Some(_), _, _) // we are in phase two and value is already decided, do nothing
        => Paxos.unchanged
    case (Phase.One, _, _, _)
        if canSendAccept // we are in phase one and have received enough promises
        => accept(value)
    case (Phase.One, _, Some(p1), Some(p2))
        if p1.proposalNumber == p2.proposalNumber // my proposal is already the highest or there is a draw
        => Paxos.unchanged
    case _ // we try to propose new value
        => prepare()

  def read: Option[A] =
    val acceptedsPerProposal: Map[(Prepare, A), Set[Accepted[A]]] = accepteds.groupBy(a => (a.proposal, a.value))
    for
      ((proposal, value), votes) <- acceptedsPerProposal.maxByOption((_, a) => a.size)
      if votes.size >= quorum
    yield value
}

object Paxos {

  given lattice[A]: Lattice[Paxos[A]] with
    override def merge(left: Paxos[A], right: Paxos[A]): Paxos[A] =
      val newmembers = Lattice[Set[Uid]].merge(left.members, right.members)
      assert(
        newmembers == left.members,
        "cannot merge two Paxos instances with differing members"
      ) // members should remain fixed
      Paxos[A](
        prepares = left.prepares merge right.prepares,
        promises = left.promises merge right.promises,
        accepts = left.accepts merge right.accepts,
        accepteds = left.accepteds merge right.accepteds,
        members = left.members
      )

  def unchanged[A]: Paxos[A] =
    Paxos[A](
      GrowOnlySet.empty[Prepare],
      GrowOnlySet.empty[Promise[A]],
      GrowOnlySet.empty[Accept[A]],
      GrowOnlySet.empty[Accepted[A]],
      Set.empty
    )

  given Consensus[Paxos] with
    extension [A](c: Paxos[A]) override def members: Set[Uid]                         = c.members
    extension [A](c: Paxos[A]) override def read: Option[A]                           = c.read
    extension [A](c: Paxos[A]) override def write(value: A)(using LocalUid): Paxos[A] = c.write(value)
    extension [A](c: Paxos[A]) override def upkeep()(using LocalUid): Paxos[A]        = c.upkeep()
    extension [A](c: Paxos[A])
      override def reset(newMembers: Set[Uid]): Paxos[A] = Paxos.unchanged.copy(members = newMembers)
}
