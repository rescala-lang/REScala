package tests.rescala.events


import org.junit.Test
import org.junit.runner.RunWith
import org.junit.runners.Parameterized
import org.scalatest.junit.AssertionsForJUnit
import org.scalatest.mock.MockitoSugar
import rescala.engines.Engine
import rescala.graph.Struct
import rescala.propagation.Turn
import tests.rescala.JUnitParameters


object dropParam_EventTest extends JUnitParameters

@RunWith(value = classOf[Parameterized])
class dropParam_EventTest[S <: Struct](engine: Engine[S, Turn[S]]) extends AssertionsForJUnit with MockitoSugar {
  implicit val implicitEngine: Engine[S, Turn[S]] = engine
  import implicitEngine._

  @Test def handlerOf_dropParam_IsExecuted() = {
    var test = 0
    val e1 = Evt[Int]()
    val e1_drop: Event[Unit] = e1.dropParam
    e1_drop += ((x) => { test += 1; })

    e1(10)
    e1(10)
    assert(test == 2)
  }

}
