package com.github.ckuessner.encrdt.causality

import com.github.ckuessner.encrdt.causality.DotStore.DotSet
import com.github.ckuessner.encrdt.causality.impl.ArrayCausalContext

case class CausalContext(acc: ArrayCausalContext) {
  def clockOf(replicaId: String): Dot = acc.clockOf(replicaId).getOrElse(Dot(0, replicaId))

  def contains(dot: Dot): Boolean = acc.contains(dot)

  def merged(other: CausalContext): CausalContext =
    CausalContext(ArrayCausalContext.contextLattice.merged(acc, other.acc))

  def merged(other: Set[Dot]): CausalContext = merged(other)

  def add(d: Dot): CausalContext = CausalContext(acc.add(d.replicaId, d.time))
}

object CausalContext {
  def apply(): CausalContext               = CausalContext(ArrayCausalContext.empty)
  def apply(dot: Dot): CausalContext       = CausalContext(ArrayCausalContext.single(dot))
  def apply(dots: Set[Dot]): CausalContext = CausalContext(ArrayCausalContext.fromSet(dots))

  given dotSetToCausalContextConversion: Conversion[DotSet, CausalContext] = apply(_)
}
