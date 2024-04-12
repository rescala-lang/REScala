package tests.rescala.testtools

import reactives.core.CreationTicket
import reactives.operator.Interface
import reactives.default.global.State as BundleState

class ReevaluationBundle[T <: Interface](val api: T) {
  import api._

  class ReevaluationTracker[A, R[_]](reactive: R[A])(implicit turnSource: CreationTicket[BundleState])
      extends munit.FunSuite {

    var results: List[A] = Nil
    /* should be private but is unused */
    // to prevent fake observers from being prematurely gc'd
    var strongRef: AnyRef = (reactive: @unchecked) match {
      case signal: Signal[_] => signal.map(reev)
      case event: Event[_]   => event.map(reev)
    }
    def reev(v1: Any): Any = {
      results ::= v1.asInstanceOf[A]
      v1
    }
    def assert(elements: A*)(using munit.Location): Unit = {
      assertEquals(results, elements.toList)
      ()
    }
    def assertClear(elements: A*)(implicit pos: munit.Location): Unit = {
      assert(elements*)
      results = Nil
    }
  }
}
