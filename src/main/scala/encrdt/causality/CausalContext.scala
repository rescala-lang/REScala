package de.ckuessner
package encrdt.causality

import encrdt.causality.DotStore.{Dot, DotSet}

// Can be optimized using Concise Version Vectors / Interval Version Vectors
case class CausalContext(dots: Set[Dot] = Set()) {
  def clockOf(replicaId: String): Dot = {
    dots
      .filter(dot => dot.replicaId == replicaId)
      .maxByOption(dot => dot.time)
      .getOrElse(LamportClock(0, replicaId))
  }

  def contains(dot: Dot): Boolean = dots.contains(dot)

  def merged(other: CausalContext): CausalContext = merged(other.dots)

  def merged(other: Set[Dot]): CausalContext = CausalContext(dots ++ other)
}

object CausalContext {

  import scala.language.implicitConversions

  implicit def dotSetToCausalContext(dotSet: DotSet): CausalContext = CausalContext(dotSet)
}