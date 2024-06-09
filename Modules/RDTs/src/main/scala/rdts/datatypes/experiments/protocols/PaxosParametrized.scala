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

case class PaxosParm[A](
    prepares: GrowOnlySet[Prepare],
    promises: GrowOnlySet[Promise[A]],
    accepts: GrowOnlySet[Accept[A]],
    accepteds: GrowOnlySet[Accepted[A]],
    members: Set[Uid] // fixed!
) {
  private def quorum: Int = members.size / 2 + 1

  def prepare()(using LocalUid, Bottom[A]): PaxosParm[A] =
    val proposalNumber = prepares.map(_.proposalNumber).maxOption.getOrElse(-1) + 1
    val prepare        = Prepare(proposalNumber)

    PaxosParm.unchanged.copy(
      prepares = prepares.insert(prepare)
    )

  def promise(prepareId: Int)(using LocalUid): PaxosParm[A] =
    val highestProposal        = prepares.maxByOption(_.proposalNumber)
    val myHighestPromiseNumber = promises.filter(_.acceptor == replicaId).map(_.proposalNumber).maxOption.getOrElse(-1)
    // check if I already promised for an equally high id
    if myHighestPromiseNumber >= highestProposal.map(_.proposalNumber).getOrElse(-1)
    then
      // already promised for equally high id
      PaxosParm.unchanged
    else
      // there is a new higher proposal
      // check if I already accepted a specific value
      val value =
        accepteds.filter(_.acceptor == replicaId).map(_.value).headOption
      PaxosParm.unchanged.copy(
        promises = promises.insert(Promise(highestProposal.get.proposalNumber, value, replicaId))
      )

  def accept(v: A)(using LocalUid): PaxosParm[A] =
    val highestProposalNumber = promises.map(_.proposalNumber).maxOption
    val promisesForProposal   = promises.filter(_.proposalNumber == highestProposalNumber.getOrElse(-1))
    // check if accepted
    if promisesForProposal.size < quorum then
      // is not accepted
      PaxosParm.unchanged
    else
      // is accepted, check if promise contains value
      val promisesWithVal = promisesForProposal.filter(_.value.isDefined)
      val value: A        = promisesWithVal.map(_.value).headOption.flatten.getOrElse(v)
      PaxosParm.unchanged.copy(
        accepts = accepts.insert(Accept(highestProposalNumber.get, value))
      )

  def accepted()(using LocalUid): PaxosParm[A] =
    // get highest accept message
    val highestAccept = accepts.maxByOption(_.proposalNumber)
    if highestAccept.isEmpty || // there are no accepts
      // I have already promised a higher proposalNumber
      promises.filter(_.acceptor == replicaId).map(_.proposalNumber).maxOption.getOrElse(-1) >
      highestAccept.get.proposalNumber
    then
      PaxosParm.unchanged
    else
      PaxosParm.unchanged.copy(accepteds =
        accepteds.insert(Accepted(
          proposalNumber = highestAccept.get.proposalNumber,
          value = highestAccept.get.value,
          acceptor = replicaId
        ))
      )

  def upkeep()(using LocalUid): PaxosParm[A] =
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
      PaxosParm.unchanged

  def write(value: A): PaxosParm[A] =
    // TODO: What would write look like? Maybe return false if we can't write at the moment?
    ???

  def read: Option[A] =
    val acceptedsPerProposal: Map[(Int, A), Set[Accepted[A]]] = accepteds.groupBy(a => (a.proposalNumber, a.value))
    for
      ((id, value), votes) <- acceptedsPerProposal.maxByOption((_, a) => a.size)
      if votes.size >= quorum
    yield value
}

object PaxosParm {

  given lattice[A]: Lattice[PaxosParm[A]] with
    override def merge(left: PaxosParm[A], right: PaxosParm[A]): PaxosParm[A] =
      PaxosParm[A](
        prepares = left.prepares merge right.prepares,
        promises = left.promises merge right.promises,
        accepts = left.accepts merge right.accepts,
        accepteds = left.accepteds merge right.accepteds,
        members = left.members
      )

  // modify
  // delta?

  def unchanged[A]: PaxosParm[A] =
    PaxosParm[A](
      GrowOnlySet.empty[Prepare],
      GrowOnlySet.empty[Promise[A]],
      GrowOnlySet.empty[Accept[A]],
      GrowOnlySet.empty[Accepted[A]],
      Set.empty[Uid]
    )
}
