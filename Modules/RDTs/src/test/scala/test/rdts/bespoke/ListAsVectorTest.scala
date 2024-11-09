package test.rdts.bespoke

import rdts.base.*
import rdts.base.Lattice.syntax.merge

class ListAsVectorTest extends munit.FunSuite {

  test("basic") {
    val l1 = List(1, 2, 3, 4)
    val l2 = l1.tail

    given Lattice[Int] = math.max

    val res = l1.merge(l2)
    assertEquals(res, List(2, 3, 4, 4))
  }

}
