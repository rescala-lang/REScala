package tests.rescala

import org.junit.Test
import org.junit.runner.RunWith
import org.junit.runners.Parameterized
import org.scalatest.junit.AssertionsForJUnit
import org.scalatest.mock.MockitoSugar
import rescala.engines.Engine
import rescala.graph.Struct
import rescala.propagation.Turn

object EmptySignalTestSuite extends JUnitParameters

@RunWith(value = classOf[Parameterized])
class EmptySignalTestSuite[S <: Struct](engine: Engine[S, Turn[S]]) extends AssertionsForJUnit with MockitoSugar {
  implicit val implicitEngine: Engine[S, Turn[S]] = engine
  import engine._


  @Test def basicEmptySignalTest(): Unit = {

    val v = Var.empty[Int]

    intercept[NoSuchElementException](v.now)

    var res = -100

    v.observe(res = _)

    assert(res == -100, "sanity")

    val s = v.map(1.+)

    intercept[NoSuchElementException](s.now)

    v.set(100)

    assert(res == 100, "observed?")
    assert(v.now == 100, "changed from empty to value")
    assert(s.now == 101, "changed from empty to value 2")



  }

}
