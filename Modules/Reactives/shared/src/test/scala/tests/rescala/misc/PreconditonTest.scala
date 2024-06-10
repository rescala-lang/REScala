package tests.rescala.misc

import reactives.operator.{Signal, Var}
import tests.rescala.testtools.FunSuiteInvertedAssert

import scala.concurrent.Future

class PreconditonTest extends munit.FunSuite {

  test("basic usage") {

    val someSource = Var(42)

    val prec1 = reactives.extra.precondition.Precondition.prepare(someSource.value == 42)

    reactives.default.transaction(prec1.accessed*) { at ?=>
      val res = prec1.check(using at)
      assert(res)
    }

    val someOtherSoure = Var(0)
    val prec2 = reactives.extra.precondition.Precondition.prepare((in: Signal[Int]) => someSource.value == in.value)

    reactives.default.transaction(prec1.accessed ++ prec2.accessed*) { at ?=>
      val res = prec2.check(using at)

      someOtherSoure.set(42)

      assert(!res(someOtherSoure))
    }

    reactives.default.transaction(prec1.accessed ++ prec2.accessed*) { at ?=>
      val res = prec2.check(using at)
      assert(res(someOtherSoure))
    }

  }

}
