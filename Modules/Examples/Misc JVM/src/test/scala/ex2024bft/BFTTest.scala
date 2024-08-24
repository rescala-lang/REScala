package ex2024bft

import rdts.base.*
import rdts.datatypes.*

class BFTTest extends munit.ScalaCheckSuite {

  given Byteable[GrowOnlyCounter]     = (obj: GrowOnlyCounter) => obj.inner.toString.getBytes
  given Lattice[BFT[GrowOnlyCounter]] = BFT.lattice

  test("basic update") {
    val id1 = LocalUid.gen()

    val bottom = BFT(summon[Bottom[GrowOnlyCounter]].empty)

    val u1 = bottom.update(_.inc()(using id1))

    val res = bottom.merge(u1)

    assertEquals(bottom.value.value, 0)
    assertEquals(u1.value.value, 0)
    assertEquals(res.value.value, 1)

    assertEquals(u1.deltas.toList(0).predecessors.toList(0), bottom.deltas.toList(0).hash)

    assertEquals(bottom.deltas.size, 1)
    assertEquals(u1.deltas.size, 1)
    assertEquals(res.deltas.size, 2)
  }

}
