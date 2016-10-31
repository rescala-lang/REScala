package tests.rescala.meta

import org.scalatest.FunSuite

class PrintTest extends FunSuite {
  import rescala.engines.CommonEngines.synchron
  implicit val reifier = new rescala.meta.EngineReifier

  /*test("print test") {

    val g = new ReactiveGraph()
    val api = new Api.metaApi(g)

    val v = api.Var(1)
    val e = v.changed
    val v2 = e.fold(0)(_ + _)
    val e2 = e || v2.changed

    DOTPrint.optimize(g)
  }*/
}
