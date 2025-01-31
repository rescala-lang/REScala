package tests.rescala.testtools

import munit.diff.DiffOptions
import munit.{Assertions, Compare, Location}

abstract class FunSuiteInvertedAssert extends munit.FunSuite {

  override def assertEquals[A, B](
      obtained: A,
      expected: B,
      clue: => Any = "values are not the same",
  )(implicit
      loc: Location,
      compare: Compare[A, B],
      diffOptions: DiffOptions,
  ): Unit =
    super.assertEquals(expected, obtained, clue)(
      loc,
      new Compare[B, A] {
        override def isEqual(obtained: B, expected: A): Boolean = compare.isEqual(expected, obtained)

        override def failEqualsComparison(
            obtained: B,
            expected: A,
            title: Any,
            assertions: Assertions,
        )(implicit loc: Location, options: DiffOptions): Nothing =
          compare.failEqualsComparison(expected, obtained, title, assertions)(using loc, options)
      }
    )

}
