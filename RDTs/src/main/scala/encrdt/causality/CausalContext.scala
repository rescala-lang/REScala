package de.ckuessner
package encrdt.causality

import encrdt.causality.DotStore.{Dot, DotSet}
import encrdt.causality.impl.ArrayCausalContext

// Can be optimized using Concise Version Vectors / Interval Version Vectors
case class CausalContext(acc: ArrayCausalContext) {
  def clockOf(replicaId: String): Dot = acc.clockOf(replicaId).getOrElse(LamportClock(0, replicaId))

  def contains(dot: Dot): Boolean = acc.contains(dot)

  def merged(other: CausalContext): CausalContext =
    CausalContext(ArrayCausalContext.contextLattice.merged(acc, other.acc))

  def merged(other: Set[Dot]): CausalContext = merged(other)

  def add(d: Dot) = CausalContext(acc.add(d.replicaId, d.time))
}

object CausalContext {

  import scala.language.implicitConversions

  def apply(dots: Set[Dot]): CausalContext = CausalContext(ArrayCausalContext.fromSet(dots))
  def apply(): CausalContext               = CausalContext(ArrayCausalContext.empty)

  implicit def dotSetToCausalContext(dotSet: DotSet): CausalContext = apply(dotSet)
}
