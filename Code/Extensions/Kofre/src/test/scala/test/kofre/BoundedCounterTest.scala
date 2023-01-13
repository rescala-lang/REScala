package test.kofre

import kofre.datatypes.experiments.BoundedCounter
import kofre.syntax.TestReplica

class BoundedCounterTest extends munit.FunSuite {

  test("basic usage") {
    val r1 = TestReplica("r1", BoundedCounter.init(100, "r1")).addParticipants(Set("r2", "r3"))
    val r2 = TestReplica("r2", r1.anon)
    val r3 = TestReplica("r3", r1.anon)

    inline def assertInvariant() =
      assert(r1.available + r2.available + r3.available <= 100)
      r1.invariantOk
      r2.invariantOk
      r3.invariantOk

    assertInvariant()

    assertEquals(r1.reserved, 100)
    assertEquals(r2.reserved, 0)
    assertEquals(r3.reserved, 0)

    r1.rebalance.allocate(10)
    assertInvariant()
    assertEquals(r1.available, 40, "100 -> rebalanced to 50 -> -10 allocated")

    r2.apply(r1.anon).rebalance
    assertInvariant()
    assertEquals(r2.available, 25, "50 transferred from r1 -> 25 rebalanced to r3")

    r3.apply(r2.anon)
    assertInvariant()
    assertEquals(r3.available, 25, "got 25 from r2")

    r1.apply(r2.anon).rebalance
    assertInvariant()

    assertEquals(r1.available, 33)
    assertEquals(r2.available, 25)
    assertEquals(r3.available, 25)

    r3.allocate(30) // does nothing
    assertInvariant()
    assertEquals(r3.available, 25)
    r3.allocate(25)
    assertInvariant()
    assertEquals(r3.available, 0)
    r1.apply(r3.anon)
    assertInvariant()
    r2.apply(r3.anon)
    assertInvariant()

    assertEquals(r1.available, 33)
    assertEquals(r2.available, 25)
    assertEquals(r3.available, 0)

    r1.rebalance
    assertInvariant()
    r3.apply(r1.anon)
    assertInvariant()

    assertEquals(r1.available, 17)
    assertEquals(r2.available, 25)
    assertEquals(r3.available, 16)

  }

}
