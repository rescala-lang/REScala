package kofre.primitives

import scala.math.Ordered.orderingToOrdered

// Lamport clock with replicaId
case class LamportClock(replicaId: String, time: Long) {
  def advance(replicaId: String): LamportClock = LamportClock(replicaId, time + 1)
}

