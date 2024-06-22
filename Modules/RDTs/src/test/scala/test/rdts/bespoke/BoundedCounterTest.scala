package test.rdts.bespoke

import rdts.datatypes.experiments.BoundedCounter
import rdts.syntax.TestReplica
import test.rdts.given

class BoundedCounterTest extends munit.FunSuite {

  test("basic usage") {
    val r1 = TestReplica("r1", BoundedCounter.init(100, "r1")).mod(_.addParticipants(Set("r2", "r3")))
    val r2 = TestReplica("r2", r1.anon)
    val r3 = TestReplica("r3", r1.anon)

    inline def assertInvariant() =
      assert(r1.anon.available(using r1.replicaId) + r2.anon.available(using r2.replicaId) + r3.anon.available(using
      r3.replicaId) <= 100)
      r1.anon.invariantOk
      r2.anon.invariantOk
      r3.anon.invariantOk

    assertInvariant()

    assertEquals(r1.anon.reserved(using r1.replicaId), 100)
    assertEquals(r2.anon.reserved(using r2.replicaId), 0)
    assertEquals(r3.anon.reserved(using r3.replicaId), 0)

    r1.mod(_.rebalance).mod(_.allocate(10))
    assertInvariant()
    assertEquals(r1.anon.available(using r1.replicaId), 40, "100 -> rebalanced to 50 -> -10 allocated")

    r2.apply(r1.anon).mod(_.rebalance)
    assertInvariant()
    assertEquals(r2.anon.available(using r2.replicaId), 25, "50 transferred from r1 -> 25 rebalanced to r3")

    r3.apply(r2.anon)
    assertInvariant()
    assertEquals(r3.anon.available(using r3.replicaId), 25, "got 25 from r2")

    r1.apply(r2.anon).mod(_.rebalance)
    assertInvariant()

    assertEquals(r1.anon.available(using r1.replicaId), 33)
    assertEquals(r2.anon.available(using r2.replicaId), 25)
    assertEquals(r3.anon.available(using r3.replicaId), 25)

    r3.mod(_.allocate(30)) // does nothing
    assertInvariant()
    assertEquals(r3.anon.available(using r3.replicaId), 25)
    r3.mod(_.allocate(25))
    assertInvariant()
    assertEquals(r3.anon.available(using r3.replicaId), 0)
    r1.apply(r3.anon)
    assertInvariant()
    r2.apply(r3.anon)
    assertInvariant()

    assertEquals(r1.anon.available(using r1.replicaId), 33)
    assertEquals(r2.anon.available(using r2.replicaId), 25)
    assertEquals(r3.anon.available(using r3.replicaId), 0)

    r1.mod(_.rebalance)
    assertInvariant()
    r3.apply(r1.anon)
    assertInvariant()

    assertEquals(r1.anon.available(using r1.replicaId), 17)
    assertEquals(r2.anon.available(using r2.replicaId), 25)
    assertEquals(r3.anon.available(using r3.replicaId), 16)

  }

}
