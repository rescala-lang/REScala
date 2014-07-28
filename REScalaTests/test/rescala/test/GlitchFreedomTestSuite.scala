package rescala.test


import org.junit.After
import org.junit.Before
import org.junit.Test
import org.scalatest.junit.AssertionsForJUnit
import org.scalatest.mock.MockitoSugar
import scala.collection.immutable.Set
import rescala.Signal
import rescala.Var
import rescala._



class GlitchFreedomTestSuite extends AssertionsForJUnit with MockitoSugar {


  var v1: Var[Int] = _
  var v2: Var[Int] = _
  var v3: Var[Int] = _
  var s1: Signal[Int] = _
  var s2: Signal[Int] = _
  var s3: Signal[Int] = _



  @Before def initialize() {
    TS.reset()
  }
  @After def cleanup() {
    TS.reset()
  }

  @Test def noGlitchesInSimpleCase() = {

    v1 = Var(1)
    s1 = StaticSignal(v1) { 2 * v1.get }
    s2 = StaticSignal(v1) { 3 * v1.get }
    s3 = StaticSignal(s1, s2) { s1.get + s2.get }

    v1.set(3)

    assert(v1.timestamps === List(Stamp(1, 0)))
    assert(s1.timestamps match { case List(Stamp(1, x), Stamp(1, y)) if Set(1, 2, 3, 4).intersect(Set(x, y)).equals(Set(x, y)) => true })
    assert(s2.timestamps match { case List(Stamp(1, x), Stamp(1, y)) if Set(1, 2, 3, 4).intersect(Set(x, y)).equals(Set(x, y)) => true })
    assert(s3.timestamps match { case List(Stamp(1, 6), Stamp(1, 5)) => true })

  }

}
