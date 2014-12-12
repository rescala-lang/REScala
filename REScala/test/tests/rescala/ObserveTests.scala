package tests.rescala

import org.junit.Test
import org.scalatest.junit.AssertionsForJUnit
import rescala.Var
import rescala.turns.Engines.default

class ObserveTests extends AssertionsForJUnit {

  @Test def canObserveSignals(): Unit = {
    var result = List[Int]()
    val v1 = Var(0)
    v1.observe(result ::= _)

    assert(result === List(0))

    v1.set(10)

    assert(result === List(10, 0))
  }

}
