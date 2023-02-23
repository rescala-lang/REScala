package tests.rescala.misc

import tests.rescala.testtools.RETests

import scala.concurrent.Future

class FromFutureTests extends RETests {
  multiEngined { engine =>
    import engine._

    import scala.concurrent.ExecutionContext.Implicits.global

    test("accessing immediately available from future value") {

      val res = Signal.fromFuture(Future.successful("immediate"))

      assert(res.readValueOnce == "immediate")

    }

  }
}
