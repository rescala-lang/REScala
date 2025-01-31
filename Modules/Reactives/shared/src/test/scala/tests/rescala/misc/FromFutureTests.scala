package tests.rescala.misc

import munit.FunSuite

import scala.concurrent.Future

class FromFutureTests extends FunSuite {

  import reactives.default.*
  {

    import scala.concurrent.ExecutionContext.Implicits.global

    test("accessing immediately available from future value") {

      val res = Signal.fromFuture(Future.successful("immediate"))

      assertEquals(res.readValueOnce, "immediate")

    }

  }
}
