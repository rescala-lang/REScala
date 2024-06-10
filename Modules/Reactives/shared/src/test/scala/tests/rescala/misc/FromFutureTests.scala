package tests.rescala.misc

import tests.rescala.testtools.FunSuiteInvertedAssert

import scala.concurrent.Future

class FromFutureTests extends FunSuiteInvertedAssert {
  import reactives.default.*
  {

    import scala.concurrent.ExecutionContext.Implicits.global

    test("accessing immediately available from future value") {

      val res = Signal.fromFuture(Future.successful("immediate"))

      assert(res.readValueOnce == "immediate")

    }

  }
}
