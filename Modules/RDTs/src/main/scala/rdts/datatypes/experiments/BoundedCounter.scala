package rdts.datatypes.experiments

import rdts.base.{Lattice, Uid}
import rdts.datatypes.experiments.BoundedCounter.neutral
import rdts.datatypes.{GrowOnlyCounter, PosNegCounter}
import rdts.syntax.LocalUid

case class BoundedCounter(reservations: PosNegCounter, allocations: GrowOnlyCounter, participants: Set[Uid]){

  type Delta = BoundedCounter

  def current = this

  def addParticipants(part: Set[Uid]): Delta = neutral.copy(participants = part)

  def allocated(id: Uid): Int = allocations.inner.getOrElse(id, 0)
  def reserved(using LocalUid): Int = reserved(LocalUid.replicaId)
  def reserved(id: Uid): Int =
    current.reservations.pos.inner.getOrElse(id, 0) - current.reservations.neg.inner.getOrElse(id, 0)
  def available(id: Uid): Int  = reserved(id) - allocated(id)
  def available(using LocalUid): Int = available(LocalUid.replicaId)

  def allocate(value: Int)(using LocalUid): Delta = {
    if value < 0 || available(LocalUid.replicaId) < value then neutral
    else neutral.copy(allocations = current.allocations.add(value))
  }

  def transfer(amount: Int, target: Uid)(using LocalUid): Delta = {
    if amount > available(LocalUid.replicaId) then neutral
    else
      neutral.copy(reservations =
        current.reservations.add(amount)(using target) merge
          current.reservations.add(-amount)
      )
  }

  def rebalance(using LocalUid): Delta = {
    val availableByReplica = current.participants.iterator.map(id => available(id) -> id).toList
    val most               = availableByReplica.max
    val least              = availableByReplica.min
    if most._2 != LocalUid.replicaId then neutral
    else
      val diff: Int = (most._1 - least._1) / 2
      current.transfer(diff, least._2)
  }

  def invariantOk: Unit =
    assert(current.reservations.value == 0, s"incorrect reservations: ${current.reservations.value}")
    assert(
      current.allocations.value <= current.reservations.neg.inner(Uid.predefined("initial-allocation")),
      s"allocation sum ${current.allocations.value} larger than initial reservations ${current.reservations.neg.inner(Uid.predefined("initial-allocation"))}"
    )
}

object BoundedCounter {
  def init(value: Int, replicaID: Uid): BoundedCounter =
    val countervalue = PosNegCounter.zero.add(-value)(using Uid.predefined("initial-allocation"))
    val initial      = PosNegCounter.zero.add(value)(using replicaID)
    BoundedCounter(countervalue merge initial, GrowOnlyCounter.zero, Set(replicaID))

  private val neutral: BoundedCounter = BoundedCounter(PosNegCounter.zero, GrowOnlyCounter.zero, Set.empty)

  given lattice: Lattice[BoundedCounter] = Lattice.derived



}
