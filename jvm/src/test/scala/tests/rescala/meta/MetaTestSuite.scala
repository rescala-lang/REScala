package tests.rescala.meta

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
    assert(g.elements.contains(v))
    assert(g.elements.contains(s3))
    assert(!g.elements.contains(s))
    assert(!g.elements.contains(s2))
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
    assert(g.elements.contains(v))
    assert(g.elements.contains(s4))
    assert(!g.elements.contains(s))
    assert(!g.elements.contains(s2a))
    assert(!g.elements.contains(s2b))
    assert(!g.elements.contains(s3))
    assert(s4.now == 5)
    v.set(1)
    assert(s4.now == 7)
  }

  @Test def autoMerging(): Unit = {
    val v = Var(0)
    val s = v.map(_ + 1)
    val s2a = s.map(_ + 1)
    val s2b = s.map(_ + 1)
    val s3 = Signals.lift(s2a, s2b) { _ + _ }
    val s4 = s3.map(_ + 1)
    val g = ReactiveGraph.fromReactive(v)
    val z = g.optimize(Set(v, s4))
    g.refresh()
    println(g.toDot())
    assert(g.elements.contains(v))
    assert(g.elements.contains(s4))
    assert(!g.elements.contains(s))
    assert(!g.elements.contains(s2a))
    assert(!g.elements.contains(s2b))
    assert(!g.elements.contains(s3))
    assert(s4.now == 5)
    v.set(1)
    assert(s4.now == 7)
  }

  @Test def autoMerging2(): Unit = {
    val v1 = Var(0)
    val v2 = Var(1)
    val s = Signals.lift(v1, v2) { _ + _ }
    val s2a = s.map(_ + 1)
    val s2b = s.map(_ + 1)
    val s3 = Signals.lift(s2a, s2b) { _ + _ }
    val s4a = s3.map(_ + 1)
    val s4b = s3.map(_ + 1)
    val g = ReactiveGraph.fromReactive(v1)
    val z = g.optimize(Set())
    g.refresh()
    println(g.toDot())
    assert(g.elements.contains(v1))
    assert(g.elements.contains(v2))
    assert(g.elements.contains(s4a))
    assert(g.elements.contains(s4b))
    assert(!g.elements.contains(s))
    assert(!g.elements.contains(s2a))
    assert(!g.elements.contains(s2b))
    assert(!g.elements.contains(s3))
    assert(s4a.now == 5)
    assert(s4b.now == 5)
    v1.set(1)
    assert(s4a.now == 7)
    assert(s4b.now == 7)
  }

}
