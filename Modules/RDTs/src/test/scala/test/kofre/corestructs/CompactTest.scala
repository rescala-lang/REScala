package test.kofre.corestructs

import kofre.base.Uid
import kofre.dotted.{Dotted, DottedLattice}
import kofre.time.{ArrayRanges, Dot, Dots, Time}
import org.scalacheck.Prop.*
import test.kofre.DataGenerator.*

class CompactTest extends munit.ScalaCheckSuite {

  test("contains works") {
    val a1                           = Uid.gen()
    given DottedLattice[Set[String]] = DottedLattice.liftLattice

    val left   = Dotted(Set("a"), Dots.empty.add(Dot(a1, 1)))
    val middle = Dotted(Set("c"), Dots.empty.add(Dot(a1, 2)))
    val right  = Dotted(Set("b"), Dots.empty.add(Dot(a1, 1)))

    val res = DottedLattice.compact(
      List(
        left,
        middle,
        right,
      ),
      Nil
    )

    assertEquals(res, List(middle, left merge right))

  }

}
