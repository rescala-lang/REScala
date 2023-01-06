package kofre.time

import kofre.base.{Id, Time}

/** A Dot is a globally unique point in time.
  * Dots are partially ordered by their time per replicaId.
  * Dots are another name for lamport clocks.
  */
case class Dot(replicaId: Id, time: Time) {
  def advance: Dot = Dot(replicaId, time + 1)
}
