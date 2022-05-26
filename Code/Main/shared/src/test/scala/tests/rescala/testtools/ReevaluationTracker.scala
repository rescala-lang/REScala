package tests.rescala.testtools

import org.scalatest.Assertions
import org.scalatest.matchers.should.Matchers
import rescala.interface.RescalaInterface

class ReevaluationBundle[T <: RescalaInterface](val api: T) {
  import api._

  class ReevaluationTracker[A, R[_]](reactive: R[A])(implicit turnSource: CreationTicket) extends Matchers {

    var results: List[A] = Nil
    /* should be private but is unused */
    // to prevent fake observers from being prematurely gc'd
    var strongRef: AnyRef = (reactive: @unchecked) match {
      case signal: Signal[_] => signal.map(reev)(turnSource)
      case event: Event[_]   => event.map(reev)(turnSource)
    }
    def reev(v1: Any): Any = {
      results ::= v1.asInstanceOf[A]
      v1
    }
    def assert(elements: A*)(implicit pos: org.scalactic.source.Position): Unit = {
      Assertions.assert(results === elements.toList)
      ()
    }
    def assertClear(elements: A*)(implicit pos: org.scalactic.source.Position): Unit = {
      assert(elements: _*)
      results = Nil
    }
  }
}
