package tests.rescala.meta

import org.scalatest.FunSuite
import rescala.api.Api
import rescala.graph.SimpleStruct
import rescala.meta._
import rescala.reactives.Var

class MetaTest extends FunSuite {
  test("meta AST creation test") {

    val g = new ReactiveGraph()

    val evt = g.createEvt[Int]()
    val evt2 = g.createEvt[Boolean]()
    val comb = ((evt.map((x : Int) => x + 1) && (_ < 0)) \ evt2).zip(evt2).fold(0)((a, b) => a + b._1)
    comb match {
      case FoldedSignalPointer(_, ZippedEventPointer(_, ExceptEventPointer(_, FilteredEventPointer(_, MappedEventPointer(_, EvtEventPointer(_), _), _), EvtEventPointer(_)), EvtEventPointer(_)), _, _) => assert(g.numNodes == 7, "graph has incorrect number of nodes")
      case _ => fail("meta AST was not correctly built!")
    }
    val snl = g.createVar[Int]()
    val snl2 = g.createVar[Char]()
    val comb2 = snl.map(toString()).delay(1).changed.switchTo(snl2)
    comb2 match {
      case SwitchToSignalPointer(_, ChangedEventPointer(_, DelayedSignalPointer(_, MappedSignalPointer(_, VarSignalPointer(_), _), 1)), VarSignalPointer(_)) => assert(g.numNodes == 13, "graph has incorrect number of nodes")
      case _ => fail("meta AST was not correctly built!")
    }
  }

  test("meta graph reification test") {
    import rescala.engines.CommonEngines.synchron

    val api = new Api.metaApi(new ReactiveGraph())
    val v = api.Var(1)
    val e = api.changed(v)
    var fired = false
    api.set(v, 2)
    e.reify(SynchronousReifier) += ((x : Int) => { fired = true })
    assert(v.reify(SynchronousReifier).now == 2, "variable set to 2")
    assert(!fired, "event not yet fired")
    v.reify(SynchronousReifier).asInstanceOf[Var[Int, SimpleStruct]].set(3)
    assert(v.reify(SynchronousReifier).now == 3, "variable set to 3")
    assert(fired, "event fired")
  }

  test("meta graph OR reification test") {
    import rescala.engines.CommonEngines.synchron

    val g = new ReactiveGraph()
    val api = new Api.metaApi(g)
    val e1 = api.Evt[Boolean]()
    val e2 = api.Evt[Boolean]()
    val e3 = api.Evt[Boolean]()
    val or = e1 || (e2, e3)
    or match {
      case OrEventPointer(_, _, _ @ _*) => assert(g.numNodes == 4, "graph has incorrect number of nodes")
    }
    var fired = 0
    or.reify(SynchronousReifier) += ((x: Any) => { fired += 1 })
    e1.reify(SynchronousReifier).fire(true)
    e2.reify(SynchronousReifier).fire(true)
    e3.reify(SynchronousReifier).fire(true)
    assert(fired == 3, "combined event didn't fire correctly")
  }
}
