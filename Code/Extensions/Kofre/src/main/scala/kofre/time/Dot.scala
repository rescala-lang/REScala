package kofre.time

import kofre.base.{Id, Time}

/** Dots are another name for lamport clocks.
  * Dots are globally unique counters that are used to track time in causal CRDTs. To guarantee global uniqueness,
  * dots combine a globally unique replicaID with a locally unique counter.
  */
case class Dot(replicaId: Id, time: Time) {
  def advance: Dot = Dot(replicaId, time + 1)
}
