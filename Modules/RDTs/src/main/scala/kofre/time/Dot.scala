package kofre.time

import kofre.base.{Uid, Time}

/** A Dot is a globally unique point in time.
  * Dots are partially ordered by their time per replicaId.
  */
case class Dot(replicaId: Uid, time: Time) {
  def advance: Dot = Dot(replicaId, time + 1)
}
