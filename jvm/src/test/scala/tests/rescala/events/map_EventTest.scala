package tests.rescala.events


import org.junit.Test
import org.junit.runner.RunWith
import org.junit.runners.Parameterized
import org.scalatest.junit.AssertionsForJUnit
import org.scalatest.mock.MockitoSugar
import rescala.graph.Spores
import rescala.propagation.Turn
import rescala.engines.Engine
import tests.rescala.JUnitParameters

object map_EventTest extends JUnitParameters

@RunWith(value = classOf[Parameterized])
class map_EventTest[S <: Spores](engine: Engine[S, Turn[S]]) extends AssertionsForJUnit with MockitoSugar {
  implicit val implicitEngine: Engine[S, Turn[S]] = engine
  import implicitEngine.{Evt}

  @Test def handlerOf_map_IsExecuted() = {
    var test = 0
    val e1 = Evt[Int]()
    val e1_map = e1 map ((x: Int) => x * 2)
    e1_map += ((x: Int) => { test += 1 })

    e1(10)
    e1(10)
    assert(test == 2)
  }

  @Test def theFunctionPassedTo_map_isApplied() = {
    var test = 0
    val e1 = Evt[Int]()
    val e1_map = e1 map ((x: Int) => x * 2)
    e1_map += ((x: Int) => { test = x })

    e1(10)
    e1(10)
    assert(test == 20)
  }

}
