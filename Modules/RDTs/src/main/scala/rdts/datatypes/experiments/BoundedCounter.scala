package rdts.datatypes.experiments

import rdts.base.{Lattice, Uid}
import rdts.datatypes.{GrowOnlyCounter, PosNegCounter}
import rdts.syntax.{LocalUid, OpsSyntaxHelper}

case class BoundedCounter(reservations: PosNegCounter, allocations: GrowOnlyCounter, participants: Set[Uid])

object BoundedCounter {
  def init(value: Int, replicaID: Uid): BoundedCounter =
    val countervalue = PosNegCounter.zero.add(-value)(using Uid.predefined("initial-allocation"))
    val initial      = PosNegCounter.zero.add(value)(using replicaID)
    BoundedCounter(countervalue merge initial, GrowOnlyCounter.zero, Set(replicaID))

  private val neutral: BoundedCounter = BoundedCounter(PosNegCounter.zero, GrowOnlyCounter.zero, Set.empty)

  given lattice: Lattice[BoundedCounter] = Lattice.derived

  extension [C](container: C)
    def boundedCounter: syntax[C] = syntax(container)

  implicit class syntax[C](container: C)
      extends OpsSyntaxHelper[C, BoundedCounter](container) {

    def addParticipants(part: Set[Uid])(using IsMutator): C = neutral.copy(participants = part).mutator

    def allocated(using IsQuery)(id: Uid): Int       = current.allocations.inner.getOrElse(id, 0)
    def reserved(using LocalUid, IsQuery): Int = reserved(replicaId)
    def reserved(using IsQuery)(id: Uid): Int =
      current.reservations.pos.inner.getOrElse(id, 0) - current.reservations.neg.inner.getOrElse(id, 0)
    def available(using IsQuery)(id: Uid): Int        = reserved(id) - allocated(id)
    def available(using LocalUid, IsQuery): Int = available(replicaId)

    def allocate(value: Int): IdMutator = {
      if value < 0 || available(replicaId) < value then neutral
      else neutral.copy(allocations = current.allocations.add(value))
    }.mutator

    def transfer(amount: Int, target: Uid): IdMutator = {
      if amount > available(replicaId) then neutral
      else
        neutral.copy(reservations =
          current.reservations.add(amount)(using target) merge
          current.reservations.add(-amount)
        )
    }.mutator

    def rebalance: IdMutator = {
      val availableByReplica = current.participants.iterator.map(id => available(id) -> id).toList
      val most               = availableByReplica.max
      val least              = availableByReplica.min
      if most._2 != replicaId then neutral
      else
        val diff: Int = (most._1 - least._1) / 2
        current.transfer(diff, least._2)
    }.mutator

    def invariantOk(using IsQuery): Unit =
      assert(current.reservations.value == 0, s"incorrect reservations: ${current.reservations.value}")
      assert(
        current.allocations.value <= current.reservations.neg.inner(Uid.predefined("initial-allocation")),
        s"allocation sum ${current.allocations.value} larger than initial reservations ${current.reservations.neg.inner(Uid.predefined("initial-allocation"))}"
      )
  }

}
