package kofre.encrdt.causality

import kofre.encrdt.causality.DotStore.{Dot, DotSet}
import kofre.primitives.Dot

// Can be optimized using Concise Version Vectors / Interval Version Vectors
case class CausalContext(dots: Set[Dot] = Set()) {
  def clockOf(replicaId: String): Dot = {
    dots
      .filter(dot => dot.replicaId == replicaId)
      .maxByOption(dot => dot.counter)
      .getOrElse(Dot(replicaId, 0))
  }

  def contains(dot: Dot): Boolean = dots.contains(dot)

  def merged(other: CausalContext): CausalContext = merged(other.dots)

  def merged(other: Set[Dot]): CausalContext = CausalContext(dots ++ other)
}

object CausalContext {

  import scala.language.implicitConversions

  implicit def dotSetToCausalContext(dotSet: DotSet): CausalContext = CausalContext(dotSet)
}
