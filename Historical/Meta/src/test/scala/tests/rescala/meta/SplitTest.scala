package tests.rescala.meta

import org.scalatest.FunSuite
import rescala.api.Api
import rescala.meta.DataFlowGraph
import rescala.meta.optimization.GraphSplit

class SplitTest extends FunSuite {
  import rescala.Schedulers.synchron
  implicit val reifier = new rescala.meta.EngineReifier

  test("Copy split test") {

    val g   = new DataFlowGraph()
    val api = new Api.metaApi(g)

    val v  = api.Var(1)
    val e  = v.changed
    val v2 = e.fold(0)(_ + _)
    val e2 = e || v2.changed

    GraphSplit.optimize(g)
    assert(g.numNodes == 0, "Original graph should be empty after copying")
    val split = GraphSplit.splittedGraphs
    assert(split.size == 1, "Connected graph should not have been split")
    assert(split.head.numNodes == 5, "Copied graph should contain all nodes")
    e2
  }

  test("Real split test") {

    val g   = new DataFlowGraph()
    val api = new Api.metaApi(g)

    val vSeperate  = api.Var(10)
    val eSeperate  = vSeperate.changed
    val eSeperate2 = api.Evt()
    val v          = api.Var(1)
    val e          = v.changed
    val v2         = e.fold(0)(_ + _)
    val e2         = e || v2.changed

    GraphSplit.optimize(g)
    assert(g.numNodes == 0, "Original graph should be empty after copying")
    val split = GraphSplit.splittedGraphs
    assert(split.size == 3, "Graph should have been split into 3 parts")
    assert(
      split.map(_.numNodes).toList.sorted == List(1, 2, 5),
      "Not all splitted graphs have the correct number of nodes"
    )
    List(eSeperate, eSeperate2, e2)
  }
}
