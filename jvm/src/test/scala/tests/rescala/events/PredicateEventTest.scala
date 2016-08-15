package tests.rescala.events


import org.junit.Test
import org.junit.runner.RunWith
import org.junit.runners.Parameterized
import org.scalatest.junit.AssertionsForJUnit
import rescala.engines.Engine
import rescala.graph.Struct
import rescala.propagation.Turn
import tests.rescala.JUnitParameters

object PredicateEventTest extends JUnitParameters

@RunWith(value = classOf[Parameterized])
class PredicateEventTest[S <: Struct](engine: Engine[S, Turn[S]]) extends AssertionsForJUnit  {
  implicit val implicitEngine: Engine[S, Turn[S]] = engine
  import implicitEngine._

  @Test def predicateEventIsExecutedOnlyIfThePredicateIsTrue(): Unit = {
    var test = 0
    var cond = false
    val e1 = Evt[Int]
    val e2 = e1 filter ((x: Int) => cond)
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
