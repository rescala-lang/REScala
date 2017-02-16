package tests.rescala.meta

import org.scalatest.FunSuite
import rescala.api.Api
import rescala.meta.optimization.RedundancyElimination
import rescala.meta.{DataFlowGraph, EngineReifier}

class RedundancyEliminationTest extends FunSuite {
  import rescala.levelbased.LevelBasedPropagationEngines.synchron
  implicit val reifier = new rescala.meta.EngineReifier

  test("Redundancy elimination test") {

    val g = new DataFlowGraph()
    val api = new Api.metaApi(g)

    var x = 0
    val v1 = api.Var(0)
    val fun = (x: Int) => x + 1
    val v2 = v1.map(fun)
    val v3 = v1.map(fun)
    val e1 = v2.changed
    val e2 = v3.changed
    assert(g.numNodes == 5, "Graph should contain 5 nodes before redundancy elimination")
    RedundancyElimination.optimize(g)
    assert(g.numNodes == 3, "Graph should contain 3 nodes after redundancy elimination")
    e1 += { x += _ }
    e2 += { x += _ }
    v1.set(1)
    assert(x == 4, "Propagation of pre-optimization values should still work correctly")
    v1.set(2)
    assert(x == 10, "Propagation of post-optimization values should work correctly")
  }

  test("No redundancy elimination test") {

    val g = new DataFlowGraph()
    val api = new Api.metaApi(g)

    var x = 0
    val v1 = api.Var(0)
    val v2 = api.Var(0)
    val fun = (x: Int) => x + 1
    val v3 = v1.map(fun)
    val v4 = v2.map(fun)
    val e1 = v3.changed
    val e2 = v4.changed
    assert(g.numNodes == 6, "Graph should contain 6 nodes before redundancy elimination")
    RedundancyElimination.optimize(g)
    assert(g.numNodes == 6, "Graph should contain 6 nodes after redundancy elimination")
    e1 += { x += _ }
    e2 += { x += _ }
    v1.set(1)
    v2.set(1)
    assert(x == 4, "Propagation of pre-optimization values should work correctly")
    v1.set(2)
    assert(x == 7, "Propagation of post-optimization values should work correctly")
  }

  test("Redundancy elimination diamond test") {

    val g = new DataFlowGraph()
    val api = new Api.metaApi(g)

    var x = 0
    val v1 = api.Var(0)
    val fun = (x: Int) => x + 1
    val v2 = v1.map(fun)
    val v3 = v1.map(fun)
    val e = v2.changed || v3.changed
    assert(g.numNodes == 6, "Graph should contain 6 nodes before redundancy elimination")
    RedundancyElimination.optimize(g)
    assert(g.numNodes == 4, "Graph should contain 4 nodes after redundancy elimination")
    e += { x += _ }
    v1.set(1)
    assert(x == 2, "Propagation of pre-optimization values should still work correctly")
    v1.set(2)
    assert(x == 5, "Propagation of post-optimization values should work correctly")
  }
}
