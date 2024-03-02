package tests.rescala.testtools

import munit.{Assertions, Compare, Location}
import reactives.operator.Interface

abstract class RETests extends munit.FunSuite {

  override def assertEquals[A, B](obtained: A, expected: B, clue: => Any)(implicit
      loc: Location,
      compare: Compare[A, B]
  ): Unit = super.assertEquals(expected, obtained, clue)(
    loc,
    new Compare[B, A] {
      override def isEqual(obtained: B, expected: A): Boolean = compare.isEqual(expected, obtained)

      override def failEqualsComparison(
          obtained: B,
          expected: A,
          title: Any,
          loc: Location,
          assertions: Assertions
      ): Nothing = compare.failEqualsComparison(expected, obtained, title, loc, assertions)
    }
  )

  def multiEngined(block: Interface => Any): Unit = block(reactives.default)

}
