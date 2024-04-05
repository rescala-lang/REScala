package com.github.ckuessner.encrdt.causality

import scala.math.Ordered.orderingToOrdered

// Lamport clock with replicaId
case class LamportClock(time: Long, replicaId: String) extends Ordered[LamportClock] {
  override def compare(that: LamportClock): Int = (time, replicaId) compare (that.time, that.replicaId)

  def advance(replicaId: String): LamportClock = LamportClock(time + 1, replicaId)
}

object LamportClock {
  def max(a: LamportClock, b: LamportClock): LamportClock = {
    if (a >= b) a
    else b
  }
}
