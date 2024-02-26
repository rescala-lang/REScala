package test.rdts.bespoke

import rdts.datatypes.experiments.BoundedCounter
import rdts.syntax.TestReplica
import rdts.time.VectorClock
import rdts.base.Uid
import rdts.base.Lattice
import test.rdts.given
import math.Ordering.Implicits.infixOrderingOps

/* This tests a pitfall, with an assumption about a causal clock with a tiebreaker */
class CausalPlusIsTieBreakerIsNonAssociative extends munit.FunSuite {

  /* Causal plus is a variant on the last-writer-wins idea, where we use a vector clock as the primary means of (causal)ordering, and when there is no causal ordering, we fall back to some tiebreaker, such as the current wallclock time.  */
  case class CausalPlus(vectorClock: VectorClock, tieBreaker: Int, value: String)

  /* The ordering is pretty standard, use the partial order first, otherwise fall back to the tiebreaker.
   * If the tiebreaker is equal, we assume the value is equal (not checked here, but also does not matter for this test.
   * However, this is actually not a valid ordering for all representable values, as shown below.
   */
  given Ordering[CausalPlus] with
    override def compare(x: CausalPlus, y: CausalPlus): Int =
      VectorClock.vectorClockOrdering.tryCompare(x.vectorClock, y.vectorClock) match
        case Some(value) => value
        case None =>
          Integer.compare(x.tieBreaker, y.tieBreaker)
  given Lattice[CausalPlus] = Lattice.fromOrdering(using summon)

  test("basic usage") {

    val a1 = Uid.predefined("a")
    val b1 = Uid.predefined("b")
    val c1 = Uid.predefined("c")

    /* The issue is, that we can define three values, that are all smaller in a cycle, thus the “ordering” is not transitive.
     * The reason this happens is because the causal order, and the tiebreaker do not agree */
    val a = CausalPlus(VectorClock(Map(a1 -> 1, c1 -> 2)), 0, "A")
    val b = CausalPlus(VectorClock(Map(b1 -> 1)), 1, "B")
    val c = CausalPlus(VectorClock(Map(c1 -> 1)), 2, "C")

    // by causal order
    assert(c < a)
    // by tiebreaker
    assert(a < b)
    // by tiebreaker
    assert(b < c)

    // without the order being correct, the lattice is also incorrect:
    val ab = a merge b
    val bc = b merge c
    val ac = a merge c

    assertNotEquals(ab merge c, a merge bc)
    assertNotEquals(ac merge b, a merge bc)

    /* Ultimately, I currently believe such a structure cannot work.
     * Specifically, if we accept the causal order as truth (if defined), then we must ensure that the tiebreaker of a larger value also dominates all tiebreakers of values that are smaller than any causally smaller values.
     * For example, the `a` above is causally ordered after the `c` but its tiebreaker is smaller that the tiebreaker of `b`.
     * As far as I can tell, the only way to fix this, is to have the tiebreaker of `a` be larger than the tiebreaker of any causally smaller values. Buf if we can achieve that, we don’t need the explicit causal order anymore, and can rely solely on the tiebreaker.
     */

  }

}
