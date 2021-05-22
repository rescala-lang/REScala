//package tests.rescala.testtools
//
//import org.scalatest.{Assertions}
//import org.scalatest.matchers.should.Matchers
//import rescala.interface.RescalaInterface
//
//class ReevaluationTracker[A] (val api: RescalaInterface) extends Matchers {
//  import api._
//  var results: List[A] = Nil
//  /* should be private but is unused */
//  var strongRef: AnyRef = _ // to prevent fake observers from being prematurely gc'd
//  def apply(signal: Signal[A])(implicit turnSource: CreationTicket) = {
//    strongRef = signal.map(reev)(turnSource)
//  }
//  def apply(event: Event[A])(implicit turnSource: CreationTicket) = {
//    strongRef = event.map(reev)(turnSource)
//  }
//  def reev(v1: A): A = {
//    results ::= v1
//    v1
//  }
//  def assert(elements: A*)(implicit pos: org.scalactic.source.Position): Unit = {
//    Assertions.assert(results === elements.toList)
//  }
//  def assertClear(elements: A*)(implicit pos: org.scalactic.source.Position): Unit = {
//    assert(elements: _*)
//    results = Nil
//  }
//}
