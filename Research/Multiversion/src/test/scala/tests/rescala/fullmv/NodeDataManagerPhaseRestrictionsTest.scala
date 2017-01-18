package tests.rescala.fullmv

import org.scalatest.{FlatSpec, Matchers}
import rescala.fullmv.api._

class NodeDataManagerPhaseRestrictionsTest extends FlatSpec with Matchers {
  def assertAllows(op: (NodeDataManager[Unit, Unit], Transaction) => Unit, when: Transaction => Boolean) = {
    assertAllows(_ => Transaction(), op, when)
    assertAllows(_ => Transaction().start(), op, when)
    assertAllows({sgt =>
      val t = Transaction()
      sgt.requireOrder(Transaction().start, t)
      t.done()
    }, op, when)
    assertAllows(_ => Transaction().done(), op, when)
  }
  def assertAllows(prepare: SerializationGraphTracking => Unit, op: (NodeDataManager[Unit, Unit], Transaction) => Unit, when: Transaction => Boolean) = {
    val sgt = SerializationGraphTracking()
    val init = Transaction().start()
    val x = NodeDataManager[Unit, Unit](sgt, init, ())
    init.done()

    val t = Transaction()
    prepare(t)

    if(when(t)) {
      op(t)
    } else {
      a [IllegalStateException] should be thrownBy {
        op(t)
      }
    }
  }

  "A node DM" should "only allow .frame by preparing transactions" in {
    assertAllows({ case (x, t) => x.frame(t) }, _.phase == Preparing)
  }
  it should "not allow .write without a frame" in {
    assertAllows({ case (x, t) => x.reevOut(t, ()) }, _ => false)
  }
  it should "allow only a single .write only for executing transactions with a frame" in {
    val b, c, d = Transaction()
    val x = NodeDataManager[Unit, Unit](SerializationGraphTracking(), d.start(), ())
    x.frame(b)
    x.frame(c)

    a [IllegalStateException] should be thrownBy {
      x.reevOut(b, ())
    }
    b.start()
    x.reevOut(b, ())
    a [IllegalStateException] should be thrownBy {
      x.reevOut(b, ())
    }

    c.done().phase === Completed
    a [IllegalStateException] should be thrownBy {
      x.reevOut(c, ())
    }
    d.done()
    c.phase === Obsolete
    a [IllegalStateException] should be thrownBy {
      x.reevOut(c, ())
    }
  }

  it should "only allow .depRead by executing transactions" in {
    assertAllows({ case (x, t) => x.depRead(t) }, _.phase == Running)
  }
  it should "only allow .old by executing transactions" in {
    assertAllows({ case (x, t) => x.old(t) }, _.phase == Running)
  }
  it should "only allow .now by executing transactions" in {
    assertAllows({ case (x, t) => x.now(t) }, _.phase == Running)
  }
  it should "only allow .after by executing transactions" in {
    assertAllows({ case (x, t) => x.after(t) }, _.phase == Running)
  }
  it should "only allow .discover by executing transactions" in {
    assertAllows({ case (x, t) => x.discover(t, ()) }, _.phase == Running)
  }
  it should "only allow .drop by executing transactions" in {
    assertAllows({ case (x, t) => x.drop(t, ()) }, _.phase == Running)
  }
}
