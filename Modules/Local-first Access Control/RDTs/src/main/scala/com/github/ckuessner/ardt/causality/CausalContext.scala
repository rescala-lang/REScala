package com.github.ckuessner.ardt.causality

import com.github.ckuessner.ardt.causality.impl.ArrayCausalContext
import DotStore.DotSet

case class CausalContext(acc: ArrayCausalContext) {
  def clockOf(replicaId: String): Dot = acc.clockOf(replicaId).getOrElse(Dot(0, replicaId))

  def contains(dot: Dot): Boolean = acc.contains(dot)

  def merged(other: CausalContext): CausalContext =
    CausalContext(ArrayCausalContext.lattice.merge(acc, other.acc))

  def merged(other: Set[Dot]): CausalContext = merged(other)

  def add(d: Dot): CausalContext = CausalContext(acc.add(d.replicaId, d.time))
}

object CausalContext {
  def apply(): CausalContext               = CausalContext(ArrayCausalContext.empty)
  def apply(dot: Dot): CausalContext       = CausalContext(ArrayCausalContext.single(dot))
  def apply(dots: Set[Dot]): CausalContext = CausalContext(ArrayCausalContext.fromSet(dots))

  given dotSetToCausalContextConversion: Conversion[DotSet, CausalContext] = apply(_)
}
