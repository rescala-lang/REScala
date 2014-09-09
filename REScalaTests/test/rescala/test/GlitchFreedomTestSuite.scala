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
import scala.language.implicitConversions


class GlitchFreedomTestSuite extends AssertionsForJUnit with MockitoSugar {


  class SuperInt(n: Int){ def in(s: Set[Int]) = s.contains(n) }
  implicit def superInt(n: Int) = new SuperInt(n: Int)


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

    assert(v1.timestamps match { case List(Stamp(1, 0)) => true })
    assert(s1.timestamps match { case List(Stamp(1, x)) if x in Set(1, 2) => true })
    assert(s2.timestamps match { case List(Stamp(1, x)) if x in Set(1, 2) => true })
    assert(s3.timestamps match { case List(Stamp(1, 3)) => true })

  }

}
