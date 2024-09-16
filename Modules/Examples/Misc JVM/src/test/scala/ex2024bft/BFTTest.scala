package ex2024bft

import rdts.base.*
import rdts.datatypes.*
import rdts.datatypes.contextual.ReplicatedList
import rdts.dotted.Dotted

class BFTTest extends munit.ScalaCheckSuite {

  given Byteable[GrowOnlyCounter]          = (obj: GrowOnlyCounter) => obj.inner.toString.getBytes
  given lat: Lattice[BFT[GrowOnlyCounter]] = BFT.lattice

  test("basic update") {
    val id1 = LocalUid.gen()

    val bottom = BFT(summon[Bottom[GrowOnlyCounter]].empty)

    val u1 = bottom.update(_.inc()(using id1))

    val res = bottom.merge(u1)

    assertEquals(bottom.value.value, 0)
    assertEquals(u1.value.value, 0)
    assertEquals(res.value.value, 1)

    assertEquals(u1.deltas.toList.head.predecessors.toList.head, bottom.deltas.toList.head.hash)

    assertEquals(bottom.deltas.size, 1)
    assertEquals(u1.deltas.size, 1)
    assertEquals(res.deltas.size, 2)
  }

  test("reject incorrect delta") {
    val id1 = LocalUid.gen()
    val id2 = LocalUid.gen()

    val bottom = BFT(summon[Bottom[GrowOnlyCounter]].empty)

    val u1          = bottom.update(_.inc()(using id1))
    val incorrectU1 = BFT(u1.deltas.map(_.copy(value = GrowOnlyCounter(Map(id2.uid -> 1)))))

    val res = bottom.merge(incorrectU1)

    assertEquals(bottom.value.value, 0)
    assertEquals(res.value.value, 0)

    assertEquals(bottom.deltas.size, 1)
    assertEquals(incorrectU1.deltas.size, 1)
  }

}
