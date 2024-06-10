package tests.rescala.testtools

import munit.{Assertions, Compare, Location}

abstract class FunSuiteInvertedAssert extends munit.FunSuite {

  override def assertEquals[A, B](obtained: A, expected: B, clue: => Any)(using
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

}
