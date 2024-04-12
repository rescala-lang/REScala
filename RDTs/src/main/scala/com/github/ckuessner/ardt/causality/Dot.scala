package com.github.ckuessner.ardt.causality

import scala.math.Ordered.orderingToOrdered

case class Dot(time: Long, replicaId: String) extends Ordered[Dot] {
  override def compare(that: Dot): Int = (time, replicaId) compare (that.time, that.replicaId)

  def advance(replicaId: String): Dot = Dot(time + 1, replicaId)
}

object Dot {
  def max(a: Dot, b: Dot): Dot = {
    if (a >= b) a
    else b
  }
}
