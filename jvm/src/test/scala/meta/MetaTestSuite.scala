package meta

import org.junit.Test
import org.junit.runner.RunWith
import org.junit.runners.Parameterized
import org.scalatest.junit.AssertionsForJUnit
import org.scalatest.mock.MockitoSugar
import rescala.engines.Engine
import rescala.graph.LevelStruct
import rescala.meta.ReactiveGraph
import rescala.propagation.Turn
import rescala.reactives.{Signals, Var}
import tests.rescala.JUnitParameters

object MetaTestSuite extends JUnitParameters

@RunWith(value = classOf[Parameterized])
class MetaTestSuite[S <: LevelStruct](engine: Engine[S, Turn[S]]) extends AssertionsForJUnit with MockitoSugar {
  implicit val implicitEngine: Engine[S, Turn[S]] = engine


  @Test def basicMerging(): Unit = {
    val v = Var(0)
    val s = v.map(_ + 1)
    val s2 = s.map(_ + 1)
    val s3 = s2.map(_ + 1)
    val g = ReactiveGraph.fromReactive(v)
    val z = g.mergeReactives(List(s, s2))
    g.refresh()
    println(g.toDot())
    assert(s3.now == 3)
    v.set(1)
    assert(s3.now == 4)
  }

  @Test def diamondMerging(): Unit = {
    val v = Var(0)
    val s = v.map(_ + 1)
    val s2a = s.map(_ + 1)
    val s2b = s.map(_ + 1)
    val s3 = Signals.lift(s2a, s2b) { _ + _ }
    val s4 = s3.map(_ + 1)
    val g = ReactiveGraph.fromReactive(v)
    val z = g.mergeReactives(List(s, s2a, s2b, s3))
    g.refresh()
    println(g.toDot())
    assert(s4.now == 5)
    v.set(1)
    assert(s4.now == 7)
  }

}
