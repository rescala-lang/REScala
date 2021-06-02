package tests.rescala.testtools

import org.scalatest.Assertions
import org.scalatest.matchers.should.Matchers
import rescala.interface.RescalaInterface

class ReevaluationBundle[T <: RescalaInterface](val api: T) {
  import api._

  class ReevaluationTracker[A] private () extends Matchers {

    import api._

    var results: List[A] = Nil
    /* should be private but is unused */
    var strongRef: AnyRef = _ // to prevent fake observers from being prematurely gc'd
    def this(signal: Signal[A])(implicit turnSource: CreationTicket) = {
      this()
      strongRef = signal.map(reev)(turnSource)
    }
    def this(event: Event[A])(implicit turnSource: CreationTicket) = {
      this()
      strongRef = event.map(reev)(turnSource)
    }
    def reev(v1: A): A = {
      results ::= v1
      v1
    }
    def assert(elements: A*)(implicit pos: org.scalactic.source.Position): Unit = {
      Assertions.assert(results === elements.toList)
    }
    def assertClear(elements: A*)(implicit pos: org.scalactic.source.Position): Unit = {
      assert(elements: _*)
      results = Nil
    }
  }
}
