package tests.rescala


import org.junit.Test
import org.junit.runner.RunWith
import org.junit.runners.Parameterized
import org.scalatest.junit.AssertionsForJUnit
import org.scalatest.mock.MockitoSugar
import rescala.turns.{Engine, Turn}
import rescala.{Signals, Var}

import scala.language.implicitConversions
object GlitchFreedomTestSuite extends JUnitParameters

@RunWith(value = classOf[Parameterized])
class GlitchFreedomTestSuite(engine: Engine[Turn]) extends AssertionsForJUnit with MockitoSugar {
  implicit val implicitEngine: Engine[Turn] = engine


  @Test def noGlitchesInSimpleCase() = {

    val v1 = Var(1)
    val s1 = v1.map { 2 * _ }
    val s2 = v1.map { 3 * _ }
    val s3 = Signals.mapping(s1, s2) { t => s1.get(t) + s2.get(t) }

    val s1List = s1.changed.list()
    val s2List = s2.changed.list()
    val s3List = s3.changed.list()

    v1.set(3)

    assert(s1List.now === List(6))
    assert(s2List.now === List(9))
    assert(s3List.now === List(15))

  }

}
