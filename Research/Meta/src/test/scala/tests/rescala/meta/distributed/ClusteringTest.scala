package tests.rescala.meta.distributed

import org.scalatest.FunSuite
import rescala.api.Api
import rescala.meta._
import rescala.meta.distributed.DistributionAnalysis

class ClusteringTest extends FunSuite {
  import rescala.engines.CommonEngines.synchron
  implicit val reifier = new rescala.meta.EngineReifier

  test("simple clustering test") {
    val g = new DataFlowGraph()
    val api = new Api.metaApi(g)
    val v1 = api.Var(0)
    val e1 = v1.changed
    val e12 = e1.map((x: Int) => x + 1)
    val v2 = api.Var(0)
    val e2 = v2.changed
    val e22 = e2.map((x: Int) => x + 1)
    val clusters = DistributionAnalysis.clusterGraph(g)
    assert(clusters.size == 2, "Graph should be clustered into 2 parts")
    assert(clusters.contains(Set(v1.tryDeref.get, e1.tryDeref.get, e12.tryDeref.get)) && clusters.contains(Set(v2.tryDeref.get, e2.tryDeref.get, e22.tryDeref.get)), "Graph should be clustered correctly")
  }

  test("complex clustering test") {
    val g = new DataFlowGraph()
    val api = new Api.metaApi(g)
    val v1 = api.Var(0)
    val v12 = v1.map((x: Int) => x + 1)
    val e1 = v1.changed
    val e12 = v12.changed
    val e13 = e1 || e12
    val v2 = api.Var(0)
    val v22 = v2.map((x: Int) => x + 1)
    val e2 = v2.changed
    val e22 = v22.changed
    val e23 = e2 || e22
    val eMix = e13 || e23
    val clusters = DistributionAnalysis.clusterGraph(g)
    assert(clusters.size == 3, "Graph should be clustered into 3 parts")
    assert(clusters.contains(Set(v1.tryDeref.get, e1.tryDeref.get, e12.tryDeref.get, v12.tryDeref.get)) && clusters.contains(Set(v2.tryDeref.get, e2.tryDeref.get, e22.tryDeref.get, v22.tryDeref.get)) && clusters.contains(Set(e13.tryDeref.get, e23.tryDeref.get, eMix.tryDeref.get)), "Graph should be clustered correctly")
  }

  test("limited clustering test") {
    val g = new DataFlowGraph()
    val api = new Api.metaApi(g)
    val v1 = api.Var(0)
    val v12 = v1.map((x: Int) => x + 1)
    val e1 = v1.changed
    val e12 = v12.changed
    val e13 = e1 || e12
    val v2 = api.Var(0)
    val v22 = v2.map((x: Int) => x + 1)
    val e2 = v2.changed
    val e22 = v22.changed
    val e23 = e2 || e22
    val eMix = e13 || e23
    val clusters = DistributionAnalysis.clusterGraph(g, 2)
    assert(clusters.size == 2, "Graph should be clustered into 2 parts")
    // Since graph is symmetric, allow both possible variations of merged clusters
    assert((clusters.contains(Set(v1.tryDeref.get, e1.tryDeref.get, e12.tryDeref.get, v12.tryDeref.get, e13.tryDeref.get, e23.tryDeref.get, eMix.tryDeref.get)) && clusters.contains(Set(v2.tryDeref.get, e2.tryDeref.get, e22.tryDeref.get, v22.tryDeref.get))) || (clusters.contains(Set(v1.tryDeref.get, e1.tryDeref.get, e12.tryDeref.get, v12.tryDeref.get)) && clusters.contains(Set(v2.tryDeref.get, e2.tryDeref.get, e22.tryDeref.get, v22.tryDeref.get, e13.tryDeref.get, e23.tryDeref.get, eMix.tryDeref.get))), "Graph should be clustered correctly")
  }
}
