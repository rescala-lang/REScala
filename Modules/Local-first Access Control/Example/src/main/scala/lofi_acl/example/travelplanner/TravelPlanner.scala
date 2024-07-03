package lofi_acl.example.travelplanner

import lofi_acl.access.Filter
import lofi_acl.ardt.datatypes.LWW.filter
import lofi_acl.ardt.datatypes.ORMap.stringKeyORMapFilter
import lofi_acl.example.travelplanner.TravelPlan.emptyStringAsBottom
import rdts.base.{Bottom, Lattice}
import rdts.datatypes.LastWriterWins
import rdts.datatypes.alternatives.ObserveRemoveSet
import rdts.datatypes.contextual.ObserveRemoveMap
import rdts.dotted.HasDots

case class TravelPlan(
    title: LastWriterWins[String],
    locations: ObserveRemoveSet[String],
    expenses: ObserveRemoveMap[String, Expense]
)

object TravelPlan {
  import rdts.datatypes.contextual.ObserveRemoveMap.lattice

  given emptyStringAsBottom: Bottom[String] = Bottom.provide("")
  // given zeroFloatAsBottom: Bottom[Float] = Bottom.provide(0.0f)

  given expenseLattice: Lattice[TravelPlan] = Lattice.derived
  given lattice: Lattice[TravelPlan]        = Lattice.derived
}

case class Expense(
    // time: LastWriterWins[LocalDateTime], // For what do we need time?
    description: LastWriterWins[String],
    amount: LastWriterWins[Option[Float]],
    comments: ObserveRemoveMap[String, LastWriterWins[String]]
) derives Lattice, HasDots, Bottom, Filter
