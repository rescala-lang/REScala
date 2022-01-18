package kofre.primitives

import scala.math.Ordered.orderingToOrdered

// Lamport clock with replicaId
case class LamportClock(time: Long, replicaId: String) {
  def advance(replicaId: String): LamportClock = LamportClock(time + 1, replicaId)
}

