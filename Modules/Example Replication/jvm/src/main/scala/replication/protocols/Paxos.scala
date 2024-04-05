package replication.protocols

import rdts.base.{Bottom, Lattice, Orderings, Uid}
import rdts.syntax.LocalReplicaId
import rdts.syntax.LocalReplicaId.replicaId
import rdts.datatypes.GrowOnlySet.*
import rdts.base.Lattice.setLattice
import rdts.datatypes.GrowOnlySet
import rdts.datatypes.GrowOnlyMap.*
import rdts.datatypes.GrowOnlyMap
import rdts.datatypes.LastWriterWins
import rdts.time.Dots
import rdts.dotted.Dotted

import scala.compiletime.{constValue, summonFrom}

// message types
case class Prepare(proposalNumber: Int)
case class Promise[A](proposalNumber: Int, value: Option[A], acceptor: Uid)
case class Accept[A](proposalNumber: Int, value: A)
case class Accepted[A](proposalNumber: Int, value: A, acceptor: Uid)

case class Paxos[A, N <: Int](
    prepares: GrowOnlySet[Prepare],
    promises: GrowOnlySet[Promise[A]],
    accepts: GrowOnlySet[Accept[A]],
    accepteds: GrowOnlySet[Accepted[A]],
)(using quorum: N) {
  private def getQuorum: N = quorum

  def prepare()(using LocalReplicaId, Bottom[A]): Paxos[A, N] =
    val proposalNumber = prepares.map(_.proposalNumber).maxOption.getOrElse(-1) + 1
    val prepare        = Prepare(proposalNumber)

    Paxos.unchanged.copy(
      prepares = prepares.insert(prepare)
    )

  def promise(prepareId: Int)(using LocalReplicaId): Paxos[A, N] =
    val highestProposal        = prepares.maxByOption(_.proposalNumber)
    val myHighestPromiseNumber = promises.filter(_.acceptor == replicaId).map(_.proposalNumber).maxOption.getOrElse(-1)
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
        promises = promises.insert(Promise(highestProposal.get.proposalNumber, value, replicaId))
      )

  def accept(v: A)(using LocalReplicaId): Paxos[A, N] =
    val highestProposalNumber = promises.map(_.proposalNumber).maxOption
    val promisesForProposal   = promises.filter(_.proposalNumber == highestProposalNumber.getOrElse(-1))
    // check if accepted
    if promisesForProposal.size < quorum then
      // is not accepted
      Paxos.unchanged
    else
      // is accepted, check if promise contains value
      val promisesWithVal = promisesForProposal.filter(_.value.isDefined)
      val value: A        = promisesWithVal.map(_.value).headOption.flatten.getOrElse(v)
      Paxos.unchanged.copy(
        accepts = accepts.insert(Accept(highestProposalNumber.get, value))
      )

  def accepted()(using LocalReplicaId): Paxos[A, N] =
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

  def upkeep()(using LocalReplicaId): Paxos[A, N] =
    // check which phase we are in
    val newestPrepare = prepares.map(_.proposalNumber).maxOption
    val newestAccept  = accepts.map(_.proposalNumber).maxOption
    if newestPrepare.getOrElse(-1) > newestAccept.getOrElse(-1) then
      // we are in promise phase
      promise(newestPrepare.get)
    else if newestAccept.isDefined then
      // we are in accepted phase
      accepted()
    else
      Paxos.unchanged

  def write(value: A): Paxos[A,N] =
    // TODO: What would write look like? Maybe return false if we can't write at the moment?
    ???

  def read: Option[A] =
    val acceptedsPerProposal: Map[(Int, A), Set[Accepted[A]]] = accepteds.groupBy(a => (a.proposalNumber, a.value))
    for
      ((id, value), votes) <- acceptedsPerProposal.maxByOption((_, a) => a.size)
      if votes.size >= quorum
    yield value
}

object Paxos {

  given lattice[A, N <: Int]: Lattice[Paxos[A, N]] with
    override def merge(left: Paxos[A, N], right: Paxos[A, N]): Paxos[A, N] =
      Paxos[A, N](
        prepares = left.prepares merge right.prepares,
        promises = left.promises merge right.promises,
        accepts = left.accepts merge right.accepts,
        accepteds = left.accepteds merge right.accepteds
      )(using left.getQuorum)

  // modify
  // delta?

  def unchanged[A, N <: Int](using quorum: N): Paxos[A, N] =
    Paxos[A, N](
      GrowOnlySet.empty[Prepare],
      GrowOnlySet.empty[Promise[A]],
      GrowOnlySet.empty[Accept[A]],
      GrowOnlySet.empty[Accepted[A]],
    )
}
