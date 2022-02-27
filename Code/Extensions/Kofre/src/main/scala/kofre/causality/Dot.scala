package kofre.causality

import kofre.Defs
import kofre.Defs.Id

/** Dots are another name for lamport clocks.
  * Dots are globally unique counters that are used to track causality in causal CRDTs. To guarantee global uniqueness,
  * dots combine a globally unique replicaID with a locally unique counter.
  */
case class Dot(replicaId: Id, time: Defs.Time) {
  def advance: Dot = Dot(replicaId, time + 1)
}
