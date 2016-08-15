package tests.rescala.events

import org.junit.Test
import org.junit.runner.RunWith
import org.junit.runners.Parameterized
import org.scalatest.junit.AssertionsForJUnit
import rescala.engines.Engine
import rescala.graph.Struct
import rescala.propagation.Turn
import tests.rescala.JUnitParameters


object AND_EventTest extends JUnitParameters

@RunWith(value = classOf[Parameterized])
class AND_EventTest[S <: Struct](engine: Engine[S, Turn[S]]) extends AssertionsForJUnit  {
  implicit val implicitEngine: Engine[S, Turn[S]] = engine
  import implicitEngine._


  @Test def handlerOf_AND_IsNOTExecutedIfEventsFireSingularly(): Unit = {
    var test = 0
    val e1 = Evt[Int]
    val e2 = Evt[Int]
    val e1_AND_e2 = e1 zip e2
    e1_AND_e2 += ((x: (Int, Int)) => { test += 1 })

    e1(10)
    e2(10)
    assert(test == 0)


  }

  @Test def handlerOf_AND_DoesNotRememberOldRounds(): Unit = {
    var test = 0
    val e1 = Evt[Int]
    val e2 = Evt[Int]
    val e1_AND_e2 = e1 zip e2
    e1_AND_e2 += ((x: (Int, Int)) => { test += 1 })

    e1(10)
    e2(10)
    e1(10)
    e2(10)
    assert(test == 0)

  }

  @Test def handlerOf_AND_IsExecutedIfBothEventsFire(): Unit = {

    var test = 0
    val e1 = Evt[Int]
    val e2 = e1 map ((x: Int) => x * 2)
    val e3 = e1 map ((x: Int) => x * 2)
    val e2_AND_e3 = e2 zip e3
    e1 += ((x: Int) => { test += 1 })
    e2 += ((x: Int) => { test += 1 })
    e3 += ((x: Int) => { test += 1 })
    e2_AND_e3 += ((x: (Int, Int)) => { test += 1 })

    e1(10)
    assert(test == 4)
  }

}
