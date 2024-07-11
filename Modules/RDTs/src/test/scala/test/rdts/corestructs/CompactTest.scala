package test.rdts.corestructs

import org.scalacheck.Prop.*
import rdts.base.{Lattice, Uid}
import rdts.dotted.Dotted
import rdts.time.{ArrayRanges, Dot, Dots, Time}
import test.rdts.DataGenerator.*

class CompactTest extends munit.ScalaCheckSuite {

  test("basic compact test") {
    val a1 = Uid.gen()

    val left   = Dotted(Set("a"), Dots.empty.add(Dot(a1, 1)))
    val middle = Dotted(Set("c"), Dots.empty.add(Dot(a1, 2)))
    val right  = Dotted(Set("b"), Dots.empty.add(Dot(a1, 1)))

    val res: List[Dotted[Set[_root_.java.lang.String]]] = Dotted.compact(
      List(
        left,
        middle,
        right,
      ),
      Nil
    )

    assertEquals(res, List(middle, left `merge` right))

  }

}
