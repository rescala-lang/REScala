package kofre.datatypes.experiments

import kofre.base.{DecomposeLattice, Id}
import kofre.datatypes.{GrowOnlyCounter, PosNegCounter}
import kofre.syntax.PermIdMutate.withID
import kofre.syntax.{ArdtOpsContains, OpsSyntaxHelper, PermId}

case class BoundedCounter(reservations: PosNegCounter, allocations: GrowOnlyCounter, participants: Set[Id])

object BoundedCounter {
  def init(value: Int, replicaID: Id): BoundedCounter =
    val countervalue = PosNegCounter.zero.add(-value)(using withID(Id.predefined("initial-allocation")))
    val initial      = PosNegCounter.zero.add(value)(using withID(replicaID))
    BoundedCounter(countervalue merge initial, GrowOnlyCounter.zero, Set(replicaID))

  private val neutral: BoundedCounter = BoundedCounter(PosNegCounter.zero, GrowOnlyCounter.zero, Set.empty)

  given lattice: DecomposeLattice[BoundedCounter] = DecomposeLattice.derived

  implicit class BoundedCounterSyntax[C](container: C)(using ArdtOpsContains[C, BoundedCounter])
      extends OpsSyntaxHelper[C, BoundedCounter](container) {

    def addParticipants(part: Set[Id])(using MutationIdP): C = neutral.copy(participants = part).mutator

    def allocated(using IdentifierP, QueryP): Int = current.allocations.inner.getOrElse(replicaID, 0)
    def reserved(using IdentifierP, QueryP): Int =
      current.reservations.pos.inner.getOrElse(replicaID, 0) - current.reservations.neg.inner.getOrElse(replicaID, 0)
    def available(using IdentifierP, QueryP): Int = reserved - allocated

    def allocate(value: Int)(using MutationIdP): C = {
      if value < 0 || available < value then neutral
      else neutral.copy(allocations = current.allocations.inc(value)(using withID(replicaID)))
    }.mutator

    def transfer(amount: Int, target: Id)(using MutationIdP): C = {
      if amount > available(using withID(replicaID)) then neutral
      else
        neutral.copy(reservations =
          current.reservations.add(amount)(using withID(target)) merge
          current.reservations.add(-amount)(using withID(replicaID))
        )
    }.mutator

    def rebalance(using MutationIdP): C = {
      val availableByReplica = current.participants.iterator.map(id => available(using withID(id)) -> id).toList
      val most               = availableByReplica.max
      val least              = availableByReplica.min
      if most._2 != replicaID then neutral
      else
        val diff: Int = (most._1 - least._1) / 2
        current.transfer(diff, least._2)(using withID(replicaID))
    }.mutator

    def invariantOk(using QueryP): Unit =
      assert(current.reservations.value == 0, s"incorrect reservations: ${current.reservations.value}")
      assert(
        current.allocations.value <= current.reservations.neg.inner(Id.predefined("initial-allocation")),
        s"allocation sum ${current.allocations.value} larger than initial reservations ${current.reservations.neg.inner(Id.predefined("initial-allocation"))}"
      )
  }

}
