package com.github.ckuessner.encrdt.lattices

import com.github.ckuessner.encrdt.causality.VectorClock
import CausalTimeTag.lwwTimeOrd

import java.time.Instant
import scala.math.PartialOrdering

case class CausalTimeTag(vectorClock: VectorClock = VectorClock(),
                         utc: Instant = Instant.ofEpochMilli(0),
                         replicaId: String = ""
                        ) extends Ordered[CausalTimeTag] {

  def advance(rId: String): CausalTimeTag = CausalTimeTag(vectorClock.advance(rId), Instant.now(), rId)

  override def compare(that: CausalTimeTag): Int = lwwTimeOrd.compare(this, that)
}

object CausalTimeTag {
  implicit def lwwTimeOrd: Ordering[CausalTimeTag] =
    (l, r) => Ordering[(VectorClock, Instant, String)].compare(unapply(l).get, unapply(r).get)

  implicit def causallyConsistentTimeReplicaOrd: Ordering[(VectorClock, Instant, String)] = (l, r) => {
    if (PartialOrdering[VectorClock].gt(l._1, r._1)) 1
    else if (PartialOrdering[VectorClock].lt(l._1, r._1)) -1
    else Ordering.by((v: (VectorClock, Instant, String)) => (v._2, v._3)).compare(l, r)
  }

  // TODO: max(vc1,vc2), max(utc1,utc2), max(rId1,rid2) would also be a plausible LUB
  implicit def semiLattice: SemiLattice[CausalTimeTag] = (l, r) =>
    if (l > r) l
    else if (l < r) r
    else if (l == r) l
    else throw new IllegalArgumentException(s"$l and $r can't be ordered")
}
