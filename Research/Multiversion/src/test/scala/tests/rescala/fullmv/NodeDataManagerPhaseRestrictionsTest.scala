package tests.rescala.fullmv

import org.scalatest.{FlatSpec, Ignore, Matchers}
import rescala.fullmv.api._

@Ignore
class NodeDataManagerPhaseRestrictionsTest extends FlatSpec with Matchers {
  def assertAllows(op: (NodeDataManager[Unit, Unit], Transaction) => Unit, when: Transaction => Boolean): Unit = {
    assertAllows(_ => Transaction(), op, when)
    assertAllows(_ => Transaction().start(), op, when)
    assertAllows({sgt =>
      val t = Transaction()
      sgt.ensureOrder(Transaction().start, t)
      t.done()
    }, op, when)
    assertAllows(_ => Transaction().done(), op, when)
  }
  def assertAllows(prepare: SerializationGraphTracking => Transaction, op: (NodeDataManager[Unit, Unit], Transaction) => Unit, when: Transaction => Boolean): Unit = {
    val sgt = SerializationGraphTracking()
    val init = Transaction().start()
    val x = NodeDataManager[Unit, Unit](sgt, init, ())
    init.done()

    val t = prepare(sgt)

    if(when(t)) {
      op(x, t)
    } else {
      a [IllegalStateException] should be thrownBy {
        op(x, t)
      }
    }
  }

  "A node DM" should "only allow .frame by preparing transactions" in {
    assertAllows({ case (x, t) => x.incrementFrame(t) }, _.phase == Preparing)
  }
  it should "not allow .write without a frame" in {
    assertAllows({ case (x, t) => x.reevOut(t, ()) }, _ => false)
  }
  it should "allow only a single .write only for executing transactions with a frame" in {
    val b, c, d = Transaction()
    val x = NodeDataManager[Unit, Unit](SerializationGraphTracking(), d.start(), ())
    x.incrementFrame(b)
    x.incrementFrame(c)

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
