package tests.rescala.meta

import org.scalatest.FunSuite
import rescala.api.Api
import rescala.meta._

class MetaTest extends FunSuite {
  import rescala.Engines.synchron
  implicit val reifier = new rescala.meta.EngineReifier

  test("meta AST creation test") {
    val g = new DataFlowGraph()

    val evt = g.createEvt[Int]()
    val evt2 = g.createEvt[Boolean]()
    val comb = ((evt.map((x : Int) => x + 1) && (_ < 0)) \ evt2).zip(evt2).fold(0)((a, b) => a + b._1)
    assert(g.numNodes == 7, "graph has incorrect number of nodes")
    val snl = g.createVar[Int]()
    val snl2 = g.createVar[Char]()
    val comb2 = snl.map(toString()).changed.switchTo(snl2)
    assert(g.numNodes == 12, "graph has incorrect number of nodes")
    List(comb, comb2)
  }

  test("meta graph reification test") {
    val api = new Api.metaApi(new DataFlowGraph())
    val v = api.Var(1)
    val e = api.changed(v)
    var fired = false
    api.set(v, 2)
    e.reify += ((x : Int) => { fired = true })
    assert(v.now == 2, "variable setting to 2 not reified")
    assert(!fired, "observer added after reification prematurly triggered")
    v.set(3)
    assert(v.now == 3, "variable setting to 3 not reified")
    assert(fired, "observer not triggered")
  }

  test("meta graph full dependency reification test") {
    val api = new Api.metaApi(new DataFlowGraph())
    val v = api.Var(1)
    val v2 = v.map((x : Int) => x + 1)
    val e = api.changed(v)
    val e2 = api.changed(v2)

    var fired = 0
    e += ((x : Int) => { fired += 1 })
    e2 += ((x : Int) => { fired += 1 })
    api.set(v, 2)

    assert(v.now == 2, "variable setting to 2 not reified")
    assert(v2.now == 3, "signal propagation to 3 not reified")
    assert(fired == 2, "event not fired or not both observers triggered")
    fired = 0
    e += ((x : Int) => { fired += 1 })
    api.set(v, 3)
    assert(v.now == 3, "variable setting to 3 not reified")
    assert(v2.now == 4, "signal propagation to 4 not reified")
    assert(fired == 3, "additionally added event not fired or not all observers triggered")
  }

  test("meta graph reification order test") {
    val api = new Api.metaApi(new DataFlowGraph())
    val v = api.Var(1)
    val v2 = v.map((x : Int) => x + 1)
    val e = api.Evt[Int]()
    val e2 = api.changed(v)
    val e3 = api.changed(v2) || e

    var fired = 0
    e2 += ((x : Int) => { fired += 1 })
    api.set(v, 2)
    e3 += ((x : Int) => { fired += 1 })

    assert(v.now == 2, "variable setting to 2 not reified")
    assert(v2.now == 3, "signal propagation to 3 not reified")
    assert(fired == 1, "observers not triggered exactly once (as only one observer was added before the event was fired)")
    fired = 0
    api.set(v, 3)
    api.fire(e, 1)
    e3 += ((x : Int) => { fired += 1 })
    assert(v.now == 3, "variable setting to 3 not reified")
    assert(v2.now == 4, "signal propagation to 4 not reified")
    assert(fired == 3, "observers not triggered exactly three times (twice through variable setting, once through event firing)")
  }

  test("meta graph OR reification test") {
    val g = new DataFlowGraph()
    val api = new Api.metaApi(g)
    val e1 = api.Evt[Boolean]()
    val e2 = api.Evt[Boolean]()
    val e3 = api.Evt[Boolean]()
    val or = e1 || (e2, e3)
    assert(g.numNodes == 4, "graph has incorrect number of nodes")
    var fired = 0
    or += ((x: Any) => { fired += 1 })
    e1.fire(true)
    e2.fire(true)
    e3.fire(true)
    assert(fired == 3, "combined event didn't fire correctly")
  }

  test("meta graph observer reification test") {
    val g = new DataFlowGraph()
    val api = new Api.metaApi(g)
    val e = api.Evt[Boolean]()
    val v = api.Var[Int](0)
    var count = 0
    var count2 = 0
    e += ((x: Boolean) => {
      count += 1
    })
    e.observe((x: Boolean) => {
      count2 += 1
    })
    v.observe((x: Int) => {
      count += x
    })
    v.observe((x: Int) => {
      count2 += x
    })
    e.fire(true)
    e.fire(true)
    assert(count == 2, "observer function was not reified correctly")
    assert(count2 == 2, "observer function was not reified correctly")
    v.set(1)
    v.set(20)
    assert(count == 23, "observer function was not reified correctly")
    assert(count2 == 23, "observer function was not reified correctly")
  }

  test("meta graph now value reification test") {
    val api = new Api.metaApi(new DataFlowGraph())
    val v = api.Var(1)
    val v2 = v.map((x : Int) => x + 1)
    api.set(v, 10)

    assert(v.now == 10, "variable value 10 not reified")
    assert(api.now(v) == 10, "variable value extraction not supported by API")
    v2
  }

  test("meta graph disconnect reification test") {
    val api = new Api.metaApi(new DataFlowGraph())
    val v = api.Var(1)
    val v2 = v.map((x : Int) => x + 1)
    val e = api.Evt[Int]()
    val e2 = api.changed(v)
    val e3 = api.changed(v2) || e

    var fired = 0
    e2 += ((x : Int) => { fired += 1 })
    api.set(v, 2)
    api.disconnectE(e2)
    api.set(v, 3)

    assert(v.now == 3, "variable setting to 3 not reified")
    assert(v2.now == 4, "signal propagation to 4 not reified")
    assert(fired == 1, "observer not triggered exactly once (only before event is disconnected)")

    e3
  }
}
