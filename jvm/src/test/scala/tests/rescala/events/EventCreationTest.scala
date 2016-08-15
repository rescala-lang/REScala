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


object EventCreationTest extends JUnitParameters

@RunWith(value = classOf[Parameterized])
class EventCreationTest[S <: Struct](engine: Engine[S, Turn[S]]) extends AssertionsForJUnit  {
  implicit val implicitEngine: Engine[S, Turn[S]] = engine

  import implicitEngine._


  @Test def addEventAfter(): Unit = {
    var res = 0
    val e0 = Evt[Int]
    val e1 = e0.map(identity)
    e1.map(_ => e0.map {_ + 1}.observe {res = _})
    e0(10)

    assert(res === 11)

  }

}
