package tests.rescala.events


import org.junit.Test
import org.scalatest.junit.AssertionsForJUnit
import org.scalatest.mock.MockitoSugar
import rescala.Evt
import rescala.Implicits.default



class PredicateEventTest extends AssertionsForJUnit with MockitoSugar {

  @Test def predicateEventIsExecutedOnlyIfThePredicateIsTrue() = {
    var test = 0
    var cond = false
    val e1 = new Evt[Int]()
    val e2 = e1 && ((x: Int) => cond)
    e2 += ((x: Int) => { test += 1 })

    e1(10)
    e1(10)
    e1(10)
    assert(test == 0)
    cond = true
    e1(10)
    //e1(10)
    //assert(test == 2)
  }


}
