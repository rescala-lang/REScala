package tests.rescala.testtools

import org.scalatest.{Assertions, Matchers}
import rescala.core.{CreationTicket, Struct}
import rescala.reactives.{Event, Signal}

class ReevaluationTracker[A, S <: Struct] private() extends Matchers {
  var results: List[A] = Nil
  /* should be private but is unused */ var strongRef: AnyRef = _ // to prevent fake observers from being prematurely gc'd
  def this(signal: Signal[A, S])(implicit turnSource: CreationTicket[S]) = {
    this()
    strongRef = signal.map(reev)(turnSource)
  }
  def this(event: Event[A, S])(implicit turnSource: CreationTicket[S]) = {
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
    assert(elements:_*)
    results = Nil
  }
}
