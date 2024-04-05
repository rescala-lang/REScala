package replication.protocols

import rdts.base.{Bottom, Lattice, Orderings, Uid}
import rdts.syntax.LocalReplicaId
import rdts.syntax.LocalReplicaId.replicaId
import rdts.datatypes.GrowOnlySet._
import rdts.base.Lattice.setLattice
import rdts.datatypes.GrowOnlySet
import rdts.datatypes.GrowOnlyMap.*
import rdts.datatypes.GrowOnlyMap
import rdts.datatypes.LastWriterWins
import rdts.time.Dots
import rdts.dotted.Dotted

// message types
case class Prepare(proposalNumber: Int)
case class Promise[A](proposalNumber: Int, value: Option[A], acceptor: Uid)
case class Accept[A](proposalNumber: Int, value: A)
case class Accepted[A](proposalNumber: Int, value: A, acceptor: Uid)

case class Paxos[A](
    prepares: GrowOnlySet[Prepare],
    promises: GrowOnlySet[Promise[A]],
    accepts: GrowOnlySet[Accept[A]],
    accepteds: GrowOnlySet[Accepted[A]]
) {
  val quorum = 2

  def prepare(using LocalReplicaId, Bottom[A])(): Paxos[A] =
    val proposalNumber = prepares.map(_.proposalNumber).maxOption.getOrElse(-1) + 1
    // val p3      = proposedValues + ((replicaId, id) -> LastWriterWins.now(value))
    val prepare = Prepare(proposalNumber)

    Paxos.unchanged.copy(
      prepares = prepares.insert(prepare)
    )

  def promise(using LocalReplicaId)(prepareId: Int): Paxos[A] =
    val highestProposal = prepares.maxBy(_.proposalNumber)
    // check if I already promised for an equally high id
    if promises.filter(_.acceptor == replicaId).maxBy(_.proposalNumber).proposalNumber >= highestProposal.proposalNumber
    then
      // already promised for equally high id
      Paxos.unchanged
    else
      // there is a new higher proposal
      // check if I already accepted a specific value
      val value =
        accepteds.filter(p => (p.acceptor == replicaId)).map(_.value).headOption
      Paxos.unchanged.copy(
        promises = promises.insert(Promise(highestProposal.proposalNumber, value, replicaId))
      )

  def accept(using LocalReplicaId)(v: A): Paxos[A] =
    val highestProposalNumber     = promises.map(_.proposalNumber).maxOption
    val promisesForProposal = promises.filter(_.proposalNumber == highestProposalNumber.getOrElse(-1))
    // check if accepted
    if promisesForProposal.size < quorum then
      // is not accepted
      Paxos.unchanged
    else
      // is accepted, check if promise contains value
      val promisesWithVal = promisesForProposal.filter(_.value.isDefined)
      val value           = promisesWithVal.map(_.value).head.getOrElse(v)
      Paxos.unchanged.copy(
        accepts = accepts.insert(Accept(highestProposalNumber.get, value))
      )

  def accepted(using LocalReplicaId): Paxos[A] =
    // get highest accept message
    val highestAccept = accepts.maxByOption(_.proposalNumber)
    if highestAccept.isEmpty || // there are no accepts
      // I have already promised a higher proposalNumber
      promises.filter(_.acceptor == replicaId).map(_.proposalNumber).maxOption.getOrElse(-1) >
      highestAccept.get.proposalNumber
    then
      Paxos.unchanged
    else
      Paxos.unchanged.copy(accepteds =
        accepteds.insert(Accepted(
          proposalNumber = highestAccept.get.proposalNumber,
          value = highestAccept.get.value,
          acceptor = replicaId
        ))
      )
}

object Paxos {
  given lattice[A]: Lattice[Paxos[A]]               = Lattice.derived
  given dottedLattice[A]: Lattice[Dotted[Paxos[A]]] = Lattice.derived

//  given Bottom[Int] with
//    override def empty: Int = Int.MinValue

  def unchanged[A]: Paxos[A] =
    Paxos[A](
      GrowOnlySet.empty[Prepare],
      GrowOnlySet.empty[Promise[A]],
      GrowOnlySet.empty[Accept[A]],
      GrowOnlySet.empty[Accepted[A]]
    )
}
