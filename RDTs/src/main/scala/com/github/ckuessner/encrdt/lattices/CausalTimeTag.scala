package com.github.ckuessner.encrdt.lattices

import CausalTimeTag.lwwTimeOrd
import com.github.ckuessner.ardt.causality.VectorClock

import java.time.Instant
import scala.math.PartialOrdering

case class CausalTimeTag(
    vectorClock: VectorClock = VectorClock(),
    utc: Instant = Instant.ofEpochMilli(0),
    replicaId: String = ""
) extends Ordered[CausalTimeTag] {

  def advance(rId: String): CausalTimeTag = CausalTimeTag(vectorClock.advance(rId), Instant.now(), rId)

  override def compare(that: CausalTimeTag): Int = lwwTimeOrd.compare(this, that)
}

object CausalTimeTag {
  given lwwTimeOrd: Ordering[CausalTimeTag] = {
    case (CausalTimeTag(l_vc, l_utc, l_rId), CausalTimeTag(r_vc, r_utc, r_rId)) =>
      if (PartialOrdering[VectorClock].gt(l_vc, r_vc)) 1
      else if (PartialOrdering[VectorClock].lt(l_vc, r_vc)) -1
      else Ordering[(Instant, String)].compare((l_utc, l_rId), (r_utc, r_rId))
  }

  given causallyConsistentTimeReplicaOrd: Ordering[(VectorClock, Instant, String)] = (l, r) => {
    if (PartialOrdering[VectorClock].gt(l._1, r._1)) 1
    else if (PartialOrdering[VectorClock].lt(l._1, r._1)) -1
    else Ordering.by((v: (VectorClock, Instant, String)) => (v._2, v._3)).compare(l, r)
  }

  // TODO: max(vc1,vc2), max(utc1,utc2), max(rId1,rid2) would also be a plausible LUB
  given semiLattice: SemiLattice[CausalTimeTag] = (l, r) =>
    if (l > r) l
    else if (l < r) r
    else if (l == r) l
    else throw new IllegalArgumentException(s"$l and $r can't be ordered")
}
