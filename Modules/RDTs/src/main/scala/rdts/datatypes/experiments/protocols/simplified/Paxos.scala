package rdts.datatypes.experiments.protocols.simplified

import rdts.base.Lattice.mapLattice
import rdts.base.LocalUid.replicaId
import rdts.base.{Bottom, Lattice, LocalUid, Uid}
import rdts.datatypes.experiments.protocols.Participants.participants
import rdts.datatypes.experiments.protocols.simplified.Paxos.given
import rdts.datatypes.experiments.protocols.{Consensus, Participants}
import rdts.datatypes.{GrowOnlySet, LastWriterWins}

import scala.math.Ordering.Implicits.infixOrderingOps

// message types
case class ProposalNum(number: Int, proposer: Uid) // ballot numbers (rounds) are replica bound
case class Prepare(proposal: ProposalNum)
case class Promise[A](proposal: ProposalNum, highestAccepted: Option[(ProposalNum, A)], acceptor: Uid)
case class Accept[A](proposal: ProposalNum, value: A)
case class Accepted[A](proposal: ProposalNum, acceptor: Uid)

case class Paxos[A](
    prepares: GrowOnlySet[Prepare],
    promises: GrowOnlySet[Promise[A]],
    accepts: GrowOnlySet[Accept[A]],
    accepted: GrowOnlySet[Accepted[A]],
    members: Map[Uid, Option[LastWriterWins[A]]] // keep track of what value each member wants to propose
) {
//  override def toString: String = pprint.apply(this).render

  private def quorum: Int = members.size / 2 + 1

  def myHighestPromise(using LocalUid): Option[Promise[A]] =
    promises.filter(_.acceptor == replicaId).maxByOption(_.proposal)

  def chooseProposalNumber(using LocalUid): ProposalNum =
    val highestNum = prepares
      .map(_.proposal.number)
      .maxOption.getOrElse(-1)
    ProposalNum(highestNum + 1, replicaId)

  // phase 1a
  def prepare()(using LocalUid): Paxos[A] =
    val proposalNumber = chooseProposalNumber
    Paxos.unchanged.copy(prepares = prepares + Prepare(proposalNumber))

  // phase 1b
  def promise(proposal: ProposalNum)(using LocalUid): Paxos[A] =
    // check if I already promised for an equally high id
    if myHighestPromise.nonEmpty && myHighestPromise.get.proposal >= proposal
    then
      // already promised for equally high id, do nothing
      Paxos.unchanged
    else
      // proposal is newer
      // check if we already accepted a specific value
      val highestAcceptedProposal = accepted
        .filter(_.acceptor == replicaId)
        .maxByOption(_.proposal).map(_.proposal)
      val valueForProposal: Option[(ProposalNum, A)] =
        highestAcceptedProposal.flatMap(p =>
          accepts
            .find(_.proposal == p)
            .map(a => (p, a.value))
        )
      Paxos.unchanged.copy(
        promises = promises + Promise(proposal, valueForProposal, replicaId)
      )

  // phase 2a
  def propose(proposal: ProposalNum, v: A)(using LocalUid): Paxos[A] =
    // check if I have received enough promises and have not proposed yet
    val myPromises  = promises.filter(_.proposal == proposal)
    val hasProposed = accepts.exists(_.proposal == proposal)
    if myPromises.size >= quorum && !hasProposed then
      // check for the newest value contained in a promise
      val acceptedValue = myPromises
        .flatMap(_.highestAccepted)
        .maxByOption((p, v) => p)
        .map((p, v) => v)
      Paxos.unchanged.copy(
        accepts = accepts + Accept(proposal, acceptedValue.getOrElse(v)),
        accepted = accepted + Accepted(proposal, replicaId),              // I can safely accept my own proposal
        members = members.updated(replicaId, Some(LastWriterWins.now(v))) // remember proposed value
      )
    else
      Paxos.unchanged // quorum not reached, do nothing

  def propose(v: A)(using LocalUid): Paxos[A] =
    // find my newest proposalNum
    val proposalNum = prepares.filter(_.proposal.proposer == replicaId).maxByOption(_.proposal)
    proposalNum match
      case Some(Prepare(prop)) => propose(prop, v)
      case None                => Paxos.unchanged

  // phase 2b
  def accept(proposal: ProposalNum)(using LocalUid): Paxos[A] =
    // check if I have already promised a newer proposal
    if myHighestPromise.nonEmpty && myHighestPromise.get.proposal > proposal
    then
      Paxos.unchanged // already promised for newer proposal, do nothing
    else
      Paxos.unchanged.copy(
        accepted = accepted + Accepted(proposal, replicaId)
      )

}

object Paxos:
  def unchanged[A]: Paxos[A] =
    Paxos[A](
      GrowOnlySet.empty[Prepare],
      GrowOnlySet.empty[Promise[A]],
      GrowOnlySet.empty[Accept[A]],
      GrowOnlySet.empty[Accepted[A]],
      Map.empty
    )

  def init[A](members: Set[Uid]): Paxos[A] =
    require(members.nonEmpty, "Cannot initialize Paxos with empty set of members.")
    unchanged[A].copy(members = members.map((_, None)).toMap)

  given Ordering[ProposalNum] with
    override def compare(x: ProposalNum, y: ProposalNum): Int =
      if x.number > y.number then 1
      else if x.number == y.number then Ordering[Uid].compare(x.proposer, y.proposer)
      else -1

  given lattice[A]: Lattice[Paxos[A]] = Lattice.derived

  given consensus: Consensus[Paxos] with
    extension [A](c: Paxos[A])
      override def write(value: A)(using LocalUid, Participants): Paxos[A] =
        if c.members.contains(replicaId) then
          def becomeLeader = c.prepare().copy(members = c.members.updated(replicaId, Some(LastWriterWins.now(value))))

          val myNewestProposal = c.prepares.filter(_.proposal.proposer == replicaId).map(_.proposal).maxOption
          myNewestProposal match // check if I already have a proposal
            case Some(proposal) =>
              // check if proposing does anything (i.e. I am the leader)
              val proposed = c.propose(proposal, value)
              if Lattice[Paxos[A]].lteq(proposed, c) then
                // proposing did not work, try to become leader
                becomeLeader
              else
                proposed
            case None => becomeLeader // no proposals yet, try to become leader
        else Paxos.unchanged
    extension [A](c: Paxos[A])
      override def read(using Participants): Option[A] =
        val acceptancePerProposal: Map[ProposalNum, Set[Accepted[A]]] = c.accepted.groupBy(_.proposal)
        for
          (proposal, votes) <- acceptancePerProposal.maxByOption((_, a) => a.size)
          acceptedProposal  <- c.accepts.find(_.proposal == proposal)
          if votes.size >= c.quorum
        yield acceptedProposal.value
    extension [A](c: Paxos[A])
      override def upkeep()(using LocalUid, Participants): Paxos[A] =
        // sanity checks
        def allIds: Set[Uid] = c.prepares.map(_.proposal.proposer)
          .union(c.promises.map(_.acceptor))
          .union(c.accepts.map(_.proposal.proposer))
          .union(c.accepted.map(_.acceptor))
        assert(allIds.subsetOf(participants)) // we only have votes from members

        // check if the newest accept is newer than the newest prepare message
        (c.accepts.maxByOption(_.proposal), c.prepares.maxByOption(_.proposal)) match
          case (Some(Accept(a, _)), Some(Prepare(p))) if a >= p =>
            // we are in phase 2
            c.accept(a)
          case (_, Some(Prepare(proposal))) =>
            // we are in phase 1
            // start by promising
            val promise  = c.promise(proposal)
            val newState = Lattice[Paxos[A]].merge(c, c.promise(proposal))

            // check if we have become the leader and can start phase 2 by proposing a value
            if {
              proposal.proposer == replicaId &&                                     // we proposed the leading proposal
              newState.promises.count(_.proposal == proposal) >= newState.quorum && // it has reached a quorum
              newState.members(proposal.proposer).nonEmpty                          // we know what value to propose
            } then
              // combine deltas for promise and propose
              Lattice[Paxos[A]].merge(
                promise,
                newState.propose(proposal, newState.members(proposal.proposer).get.value)
              )
            else
              // we are not the leader, promise
              promise
          case _ =>
            // there are no prepare messages, do nothing
            Paxos.unchanged

    override def init[A](members: GrowOnlySet[Uid]): Paxos[A] = Paxos.init(members = members)

    override def empty[A]: Paxos[A] = Paxos.unchanged

    override def lattice[A]: Lattice[Paxos[A]] = Paxos.lattice

  given bottom: Bottom[Paxos[?]] with
    override def empty: Paxos[?] = unchanged
