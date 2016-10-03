package tests.rescala.meta

import org.scalatest.FunSuite
import rescala.api.Api
import rescala.engines.CommonEngines
import rescala.meta._

class MetaTest extends FunSuite {

  val SynchronousReifier = new EngineReifier()(CommonEngines.synchron)

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
    assert(v.reify(SynchronousReifier).now == 2, "variable setting to 2 not reified")
    assert(!fired, "observer added after reification prematurly triggered")
    v.reify(SynchronousReifier).set(3)
    assert(v.reify(SynchronousReifier).now == 3, "variable setting to 3 not reified")
    assert(fired, "observer not triggered")
  }

  test("meta graph full dependency reification test") {
    import rescala.engines.CommonEngines.synchron

    val api = new Api.metaApi(new ReactiveGraph())
    val v = api.Var(1)
    val v2 = v.map((x : Int) => x + 1)
    val e = api.changed(v)
    val e2 = api.changed(v2)

    var fired = 0
    e += ((x : Int) => { fired += 1 })
    e2 += ((x : Int) => { fired += 1 })
    api.set(v, 2)

    assert(v.reify(SynchronousReifier).now == 2, "variable setting to 2 not reified")
    assert(v2.reify(SynchronousReifier).now == 3, "signal propagation to 3 not reified")
    assert(fired == 2, "event not fired or not both observers triggered")
    fired = 0
    e += ((x : Int) => { fired += 1 })
    api.set(v, 3)
    assert(v.reify(SynchronousReifier).now == 3, "variable setting to 3 not reified")
    assert(v2.reify(SynchronousReifier).now == 4, "signal propagation to 4 not reified")
    assert(fired == 3, "additionally added event not fired or not all observers triggered")
  }

  test("meta graph reification order test") {
    import rescala.engines.CommonEngines.synchron

    val api = new Api.metaApi(new ReactiveGraph())
    val v = api.Var(1)
    val v2 = v.map((x : Int) => x + 1)
    val e = api.Evt[Int]()
    val e2 = api.changed(v)
    val e3 = api.changed(v2) || e

    var fired = 0
    e2 += ((x : Int) => { fired += 1 })
    api.set(v, 2)
    e3 += ((x : Int) => { fired += 1 })

    assert(v.reify(SynchronousReifier).now == 2, "variable setting to 2 not reified")
    assert(v2.reify(SynchronousReifier).now == 3, "signal propagation to 3 not reified")
    assert(fired == 1, "observers not triggered exactly once (as only one observer was added before the event was fired)")
    fired = 0
    api.set(v, 3)
    api.fire(e, 1)
    e3 += ((x : Int) => { fired += 1 })
    assert(v.reify(SynchronousReifier).now == 3, "variable setting to 3 not reified")
    assert(v2.reify(SynchronousReifier).now == 4, "signal propagation to 4 not reified")
    assert(fired == 3, "observers not triggered exactly three times (twice through variable setting, once through event firing)")
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

  test("meta graph observer reification test") {
    import rescala.engines.CommonEngines.synchron

    val g = new ReactiveGraph()
    val api = new Api.metaApi(g)
    val e = api.Evt[Boolean]()
    val v = api.Var[Int](0)
    var count = 0
    var count2 = 0
    (e += ((x: Boolean) => {
      count += 1
    })).reify(SynchronousReifier)
    e.observe((x: Boolean) => {
      count2 += 1
    }).reify(SynchronousReifier)
    v.observe((x: Int) => {
      count += x
    }).reify(SynchronousReifier)
    v.observe((x: Int) => {
      count2 += x
    }).reify(SynchronousReifier)
    e.reify(SynchronousReifier).fire(true)
    e.reify(SynchronousReifier).fire(true)
    assert(count == 2, "observer function was not reified correctly")
    assert(count2 == 2, "observer function was not reified correctly")
    v.reify(SynchronousReifier).set(1)
    v.reify(SynchronousReifier).set(20)
    assert(count == 23, "observer function was not reified correctly")
    assert(count2 == 23, "observer function was not reified correctly")
  }
}
