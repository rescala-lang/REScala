package kofre.datatypes.experiments

import kofre.base.{DecomposeLattice, Id}
import kofre.datatypes.{GrowOnlyCounter, PosNegCounter}
import kofre.syntax.{Named, OpsSyntaxHelper, PermId}
import kofre.syntax.Named.named

case class BoundedCounter(reservations: PosNegCounter, allocations: GrowOnlyCounter, participants: Set[Id])

object BoundedCounter {
  def init(value: Int, replicaID: Id): BoundedCounter =
    val countervalue = Named(Id.predefined("initial-allocation"), PosNegCounter.zero).add(-value).anon
    val initial      = Named(replicaID, PosNegCounter.zero).add(value).anon
    BoundedCounter(countervalue merge initial, GrowOnlyCounter.zero, Set(replicaID))

  private val neutral: BoundedCounter = BoundedCounter(PosNegCounter.zero, GrowOnlyCounter.zero, Set.empty)

  given lattice: DecomposeLattice[BoundedCounter] = DecomposeLattice.derived

  implicit class BoundedCounterSyntax[C](container: C)
      extends OpsSyntaxHelper[C, BoundedCounter](container) {

    def addParticipants(part: Set[Id])(using PermIdMutate): C = neutral.copy(participants = part).mutator

    def allocated(using PermId, PermQuery): Int = current.allocations.inner.getOrElse(replicaID, 0)
    def reserved(using PermId, PermQuery): Int =
      current.reservations.pos.inner.getOrElse(replicaID, 0) - current.reservations.neg.inner.getOrElse(replicaID, 0)
    def available(using PermId, PermQuery): Int = reserved - allocated

    def allocate(value: Int)(using PermIdMutate): C = {
      if value < 0 || available < value then neutral
      else neutral.copy(allocations = current.allocations.add(value))
    }.mutator

    def transfer(amount: Int, target: Id)(using PermIdMutate): C = {
      if amount > available then neutral
      else
        neutral.copy(reservations =
          current.reservations.add(amount)merge
          current.reservations.add(-amount)
        )
    }.mutator

    def rebalance(using PermIdMutate): C = {
      val availableByReplica = current.participants.iterator.map(id => available -> id).toList
      val most               = availableByReplica.max
      val least              = availableByReplica.min
      if most._2 != replicaID then neutral
      else
        val diff: Int = (most._1 - least._1) / 2
        current.transfer(diff, least._2)
    }.mutator

    def invariantOk(using PermQuery): Unit =
      assert(current.reservations.value == 0, s"incorrect reservations: ${current.reservations.value}")
      assert(
        current.allocations.value <= current.reservations.neg.inner(Id.predefined("initial-allocation")),
        s"allocation sum ${current.allocations.value} larger than initial reservations ${current.reservations.neg.inner(Id.predefined("initial-allocation"))}"
      )
  }

}
