package tests.rescala.fullmv

import org.scalatest.{FlatSpec, Matchers}
import rescala.fullmv.api._
import tests.rescala.fullmv.testutils.{TestHost, UserComputationTracker}

class NodeVersionListReevaluationTest extends FlatSpec with Matchers {
  import tests.rescala.fullmv.testutils.VersionListAsserter._
  "A version list" should "reevaluate in serializable order" in {
    val tracker = new UserComputationTracker
    val t = Transaction().start()
    val x = new SignalVersionList(TestHost, t, t, tracker.comp)
    t.done()

    val t2 = Transaction()
    t2.branches(1)
    x.incrementFrame(t2)
    t2.start()

    val t3 = Transaction()
    t3.branches(1)
    x.incrementFrame(t3)
    t3.start()

    val t4 = Transaction()
    t4.branches(1)
    x.incrementFrame(t4)
    t4.start()

    assertVersions(x,
      new Version(t, Set.empty, pending = 0, changed = 0, Some(t)),
      new Version(t2, Set.empty, pending = 1, changed = 0, None),
      new Version(t3, Set.empty, pending = 1, changed = 0, None),
      new Version(t4, Set.empty, pending = 1, changed = 0, None))
    assert(x.firstFrame == 1)
    assert(tracker.executedTransactionsInReverseOrder(x) == List())

    t3.branches(1)
    x.notify(t3, true, None)
    assertVersions(x,
      new Version(t, Set.empty, pending = 0, changed = 0, Some(t)),
      new Version(t2, Set.empty, pending = 1, changed = 0, None),
      new Version(t3, Set.empty, pending = 0, changed = 1, None),
      new Version(t4, Set.empty, pending = 1, changed = 0, None))
    assert(x.firstFrame == 1)
    assert(tracker.executedTransactionsInReverseOrder(x) == List())

    t2.branches(1)
    x.notify(t2, true, None)
    t2.done()
    t3.done()
    assertVersions(x,
      new Version(t, Set.empty, pending = 0, changed = 0, Some(t)),
      new Version(t2, Set.empty, pending = 0, changed = 0, Some(t2)),
      new Version(t3, Set.empty, pending = 0, changed = 0, Some(t3)),
      new Version(t4, Set.empty, pending = 1, changed = 0, None))
    assert(x.firstFrame == 3)
    assert(tracker.executedTransactionsInReverseOrder(x) == List(t3, t2))
    assert(tracker.receivedInputs(x)(t2) == t)
    assert(tracker.receivedInputs(x)(t3) == t2)

    t4.branches(1)
    x.notify(t4, false, None)
    t4.done()
    assertVersions(x,
      new Version(t, Set.empty, pending = 0, changed = 0, Some(t)),
      new Version(t2, Set.empty, pending = 0, changed = 0, Some(t2)),
      new Version(t3, Set.empty, pending = 0, changed = 0, Some(t3)),
      new Version(t4, Set.empty, pending = 0, changed = 0, None))
    assert(x.firstFrame == 4)
    assert(tracker.executedTransactionsInReverseOrder(x) == List(t3, t2))
  }

  it should "propagate reevaluations" in {
    // x -> y -> z
    val tracker = new UserComputationTracker
    val t = Transaction().start()
    val x, y, z = new SignalVersionList(TestHost, t, t, tracker.comp)
    x.discoverSuspend(ReevaluationTicket(t, y))
    y.discoverSuspend(ReevaluationTicket(t, z))
    t.done()

    val t2 = Transaction()
    t2.branches(1)
    x.incrementFrame(t2)
    t2.start()

    val t3 = Transaction()
    t3.branches(1)
    x.incrementFrame(t3)
    t3.start()
    t3.branches(1)
    x.notify(t3, true, None)

    assertVersions(x,
      new Version(t, Set(y), pending = 0, changed = 0, Some(t)),
      new Version(t2, Set(y), pending = 1, changed = 0, None),
      new Version(t3, Set(y), pending = 0, changed = 1, None))
    assert(x.firstFrame == 1)
    assert(tracker.executedTransactionsInReverseOrder(x) == List())
    assertVersions(y,
      new Version(t, Set(z), pending = 0, changed = 0, Some(t)),
      new Version(t2, Set(z), pending = 1, changed = 0, None))
    assert(y.firstFrame == 1)
    assert(tracker.executedTransactionsInReverseOrder(y) == List())
    assertVersions(z,
      new Version(t, Set.empty, pending = 0, changed = 0, Some(t)),
      new Version(t2, Set.empty, pending = 1, changed = 0, None))
    assert(z.firstFrame == 1)
    assert(tracker.executedTransactionsInReverseOrder(z) == List())

    t2.branches(1)
    x.notify(t2, true, None)
    t2.done()
    t3.done()

    assertVersions(x,
      new Version(t, Set(y), pending = 0, changed = 0, Some(t)),
      new Version(t2, Set(y), pending = 0, changed = 0, Some(t2)),
      new Version(t3, Set(y), pending = 0, changed = 0, Some(t3)))
    assert(x.firstFrame == 3)
    assert(tracker.executedTransactionsInReverseOrder(x) == List(t3, t2))
    assert(tracker.receivedInputs(x)(t2) == t)
    assert(tracker.receivedInputs(x)(t3) == t2)
    assertVersions(y,
      new Version(t, Set(z), pending = 0, changed = 0, Some(t)),
      new Version(t2, Set(z), pending = 0, changed = 0, Some(t2)),
      new Version(t3, Set(z), pending = 0, changed = 0, Some(t3)))
    assert(y.firstFrame == 3)
    assert(tracker.executedTransactionsInReverseOrder(y) == List(t3, t2))
    assert(tracker.receivedInputs(y)(t2) == t)
    assert(tracker.receivedInputs(y)(t3) == t2)
    assertVersions(z,
      new Version(t, Set.empty, pending = 0, changed = 0, Some(t)),
      new Version(t2, Set.empty, pending = 0, changed = 0, Some(t2)),
      new Version(t3, Set.empty, pending = 0, changed = 0, Some(t3)))
    assert(z.firstFrame == 3)
    assert(tracker.executedTransactionsInReverseOrder(z) == List(t3, t2))
    assert(tracker.receivedInputs(z)(t2) == t)
    assert(tracker.receivedInputs(z)(t3) == t2)

    val t4 = Transaction()
    t4.branches(1)
    x.incrementFrame(t4)
    t4.start()

    assertVersions(x,
      new Version(t, Set(y), pending = 0, changed = 0, Some(t)),
      new Version(t2, Set(y), pending = 0, changed = 0, Some(t2)),
      new Version(t3, Set(y), pending = 0, changed = 0, Some(t3)),
      new Version(t4, Set(y), pending = 1, changed = 0, None))
    assert(x.firstFrame == 3)
    assert(tracker.executedTransactionsInReverseOrder(x) == List(t3, t2))
    assertVersions(y,
      new Version(t, Set(z), pending = 0, changed = 0, Some(t)),
      new Version(t2, Set(z), pending = 0, changed = 0, Some(t2)),
      new Version(t3, Set(z), pending = 0, changed = 0, Some(t3)),
      new Version(t4, Set(z), pending = 1, changed = 0, None))
    assert(y.firstFrame == 3)
    assert(tracker.executedTransactionsInReverseOrder(y) == List(t3, t2))
    assertVersions(z,
      new Version(t, Set.empty, pending = 0, changed = 0, Some(t)),
      new Version(t2, Set.empty, pending = 0, changed = 0, Some(t2)),
      new Version(t3, Set.empty, pending = 0, changed = 0, Some(t3)),
      new Version(t4, Set.empty, pending = 1, changed = 0, None))
    assert(z.firstFrame == 3)
    assert(tracker.executedTransactionsInReverseOrder(z) == List(t3, t2))

    t4.branches(1)
    x.notify(t4, true, None)
    t4.done()

    assertVersions(x,
      new Version(t, Set(y), pending = 0, changed = 0, Some(t)),
      new Version(t2, Set(y), pending = 0, changed = 0, Some(t2)),
      new Version(t3, Set(y), pending = 0, changed = 0, Some(t3)),
      new Version(t4, Set(y), pending = 0, changed = 0, Some(t4)))
    assert(x.firstFrame == 4)
    assert(tracker.executedTransactionsInReverseOrder(x) == List(t4, t3, t2))
    assert(tracker.receivedInputs(x)(t4) == t3)
    assertVersions(y,
      new Version(t, Set(z), pending = 0, changed = 0, Some(t)),
      new Version(t2, Set(z), pending = 0, changed = 0, Some(t2)),
      new Version(t3, Set(z), pending = 0, changed = 0, Some(t3)),
      new Version(t4, Set(z), pending = 0, changed = 0, Some(t4)))
    assert(y.firstFrame == 4)
    assert(tracker.executedTransactionsInReverseOrder(y) == List(t4, t3, t2))
    assert(tracker.receivedInputs(y)(t4) == t3)
    assertVersions(z,
      new Version(t, Set.empty, pending = 0, changed = 0, Some(t)),
      new Version(t2, Set.empty, pending = 0, changed = 0, Some(t2)),
      new Version(t3, Set.empty, pending = 0, changed = 0, Some(t3)),
      new Version(t4, Set.empty, pending = 0, changed = 0, Some(t4)))
    assert(z.firstFrame == 4)
    assert(tracker.executedTransactionsInReverseOrder(z) == List(t4, t3, t2))
    assert(tracker.receivedInputs(z)(t4) == t3)
  }

  it should "propagate partial framings with reevaluations" in {
    // w -> x -> y -> z
    val tracker = new UserComputationTracker
    val t = Transaction().start()
    val w, x, y, z = new SignalVersionList(TestHost, t, t, tracker.comp)
    w.discoverSuspend(ReevaluationTicket(t, x))
    x.discoverSuspend(ReevaluationTicket(t, y))
    y.discoverSuspend(ReevaluationTicket(t, z))

    val t2 = Transaction()
    t2.branches(1)
    w.incrementFrame(t2)
    t2.start()

    val t3 = Transaction()
    t3.branches(1)
    w.incrementFrame(t3)
    t3.start()

    val t4 = Transaction()
    t4.branches(1)
    y.incrementFrame(t4)
    t4.start()

    assert(TestHost.sgt.ensureOrder(t3, t4) == FirstFirst)

    assertVersions(w,
      new Version(t, Set(x), pending = 0, changed = 0, Some(t)),
      new Version(t2, Set(x), pending = 1, changed = 0, None),
      new Version(t3, Set(x), pending = 1, changed = 0, None))
    assert(w.firstFrame == 1)
    assert(tracker.executedTransactionsInReverseOrder(w) == List())
    assertVersions(x,
      new Version(t, Set(y), pending = 0, changed = 0, Some(t)),
      new Version(t2, Set(y), pending = 1, changed = 0, None))
    assert(x.firstFrame == 1)
    assert(tracker.executedTransactionsInReverseOrder(x) == List())
    assertVersions(y,
      new Version(t, Set(z), pending = 0, changed = 0, Some(t)),
      new Version(t2, Set(z), pending = 1, changed = 0, None),
      new Version(t4, Set(z), pending = 1, changed = 0, None))
    assert(y.firstFrame == 1)
    assert(tracker.executedTransactionsInReverseOrder(y) == List())
    assertVersions(z,
      new Version(t, Set.empty, pending = 0, changed = 0, Some(t)),
      new Version(t2, Set.empty, pending = 1, changed = 0, None))
    assert(z.firstFrame == 1)
    assert(tracker.executedTransactionsInReverseOrder(z) == List())

    t2.branches(1)
    w.notify(t2, true, None)
    t2.done()

    assertVersions(w,
      new Version(t, Set(x), pending = 0, changed = 0, Some(t)),
      new Version(t2, Set(x), pending = 0, changed = 0, Some(t2)),
      new Version(t3, Set(x), pending = 1, changed = 0, None))
    assert(w.firstFrame == 2)
    assert(tracker.executedTransactionsInReverseOrder(w) == List(t2))
    assert(tracker.receivedInputs(w)(t2) == t)
    assertVersions(x,
      new Version(t, Set(y), pending = 0, changed = 0, Some(t)),
      new Version(t2, Set(y), pending = 0, changed = 0, Some(t2)),
      new Version(t3, Set(y), pending = 1, changed = 0, None))
    assert(x.firstFrame == 2)
    assert(tracker.executedTransactionsInReverseOrder(x) == List(t2))
    assert(tracker.receivedInputs(x)(t2) == t)
    assertVersions(y,
      new Version(t, Set(z), pending = 0, changed = 0, Some(t)),
      new Version(t2, Set(z), pending = 0, changed = 0, Some(t2)),
      new Version(t3, Set(z), pending = 1, changed = 0, None),
      new Version(t4, Set(z), pending = 1, changed = 0, None))
    assert(y.firstFrame == 2)
    assert(tracker.executedTransactionsInReverseOrder(y) == List(t2))
    assert(tracker.receivedInputs(y)(t2) == t)
    assertVersions(z,
      new Version(t, Set.empty, pending = 0, changed = 0, Some(t)),
      new Version(t2, Set.empty, pending = 0, changed = 0, Some(t2)),
      new Version(t3, Set.empty, pending = 1, changed = 0, None))
    assert(z.firstFrame == 2)
    assert(tracker.executedTransactionsInReverseOrder(z) == List(t2))
    assert(tracker.receivedInputs(z)(t2) == t)
  }

  it should "skip some framing/nochanges after a nochange catching up with partial framing" in {
    //    z
    //   / \
    //  y  |
    //  |  |
    //  x  |
    // / \/\
    // u v w
    val tracker = new UserComputationTracker
    val t = Transaction().start()
    val u, v, w, x, y, z = new SignalVersionList(TestHost, t, t, tracker.comp)
    u.discoverSuspend(ReevaluationTicket(t, x))
    v.discoverSuspend(ReevaluationTicket(t, x))
    x.discoverSuspend(ReevaluationTicket(t, y))
    v.discoverSuspend(ReevaluationTicket(t, z))
    y.discoverSuspend(ReevaluationTicket(t, z))
    w.discoverSuspend(ReevaluationTicket(t, z))

    val t2 = Transaction()
    t2.branches(1)
    u.incrementFrame(t2)
    t2.start()

    val t3 = Transaction()
    t3.branches(2)
    v.incrementFrame(t3)
    w.incrementFrame(t3)
    t3.start()

    assertVersions(u,
      new Version(t, Set(x), pending = 0, changed = 0, Some(t)),
      new Version(t2, Set(x), pending = 1, changed = 0, None))
    assert(u.firstFrame == 1)
    assert(tracker.executedTransactionsInReverseOrder(u) == List())
    assertVersions(v,
      new Version(t, Set(x, z), pending = 0, changed = 0, Some(t)),
      new Version(t3, Set(x, z), pending = 1, changed = 0, None))
    assert(v.firstFrame == 1)
    assert(tracker.executedTransactionsInReverseOrder(v) == List())
    assertVersions(w,
      new Version(t, Set(z), pending = 0, changed = 0, Some(t)),
      new Version(t3, Set(z), pending = 1, changed = 0, None))
    assert(w.firstFrame == 1)
    assert(tracker.executedTransactionsInReverseOrder(w) == List())
    assertVersions(x,
      new Version(t, Set(y), pending = 0, changed = 0, Some(t)),
      new Version(t2, Set(y), pending = 1, changed = 0, None),
      new Version(t3, Set(y), pending = 1, changed = 0, None))
    assert(x.firstFrame == 1)
    assert(tracker.executedTransactionsInReverseOrder(x) == List())
    assertVersions(y,
      new Version(t, Set(z), pending = 0, changed = 0, Some(t)),
      new Version(t2, Set(z), pending = 1, changed = 0, None))
    assert(y.firstFrame == 1)
    assert(tracker.executedTransactionsInReverseOrder(y) == List())
    assertVersions(z,
      new Version(t, Set.empty, pending = 0, changed = 0, Some(t)),
      new Version(t2, Set.empty, pending = 1, changed = 0, None),
      new Version(t3, Set.empty, pending = 2, changed = 0, None))
    assert(z.firstFrame == 1)
    assert(tracker.executedTransactionsInReverseOrder(z) == List())

    t3.branches(2)
    v.notify(t3, false, None)
    w.notify(t3, true, None)

    assertVersions(u,
      new Version(t, Set(x), pending = 0, changed = 0, Some(t)),
      new Version(t2, Set(x), pending = 1, changed = 0, None))
    assert(u.firstFrame == 1)
    assert(tracker.executedTransactionsInReverseOrder(u) == List())
    assertVersions(v,
      new Version(t, Set(x, z), pending = 0, changed = 0, Some(t)),
      new Version(t3, Set(x, z), pending = 0, changed = 0, None))
    assert(v.firstFrame == 2)
    assert(tracker.executedTransactionsInReverseOrder(v) == List())
    assertVersions(w,
      new Version(t, Set(z), pending = 0, changed = 0, Some(t)),
      new Version(t3, Set(z), pending = 0, changed = 0, Some(t3)))
    assert(w.firstFrame == 2)
    assert(tracker.executedTransactionsInReverseOrder(w) == List(t3))
    assert(tracker.receivedInputs(w)(t3) == t)
    assertVersions(x,
      new Version(t, Set(y), pending = 0, changed = 0, Some(t)),
      new Version(t2, Set(y), pending = 1, changed = 0, None),
      new Version(t3, Set(y), pending = 0, changed = 0, None))
    assert(x.firstFrame == 1)
    assert(tracker.executedTransactionsInReverseOrder(x) == List())
    assertVersions(y,
      new Version(t, Set(z), pending = 0, changed = 0, Some(t)),
      new Version(t2, Set(z), pending = 1, changed = 0, None))
    assert(y.firstFrame == 1)
    assert(tracker.executedTransactionsInReverseOrder(y) == List())
    assertVersions(z,
      new Version(t, Set.empty, pending = 0, changed = 0, Some(t)),
      new Version(t2, Set.empty, pending = 1, changed = 0, None),
      new Version(t3, Set.empty, pending = 0, changed = 1, None))
    assert(z.firstFrame == 1)
    assert(tracker.executedTransactionsInReverseOrder(z) == List())

    t2.branches(1)
    u.notify(t2, false, None) // changed=true would work equally
    t2.done()
    t3.done()

    assertVersions(u,
      new Version(t, Set(x), pending = 0, changed = 0, Some(t)),
      new Version(t2, Set(x), pending = 0, changed = 0, None))
    assert(u.firstFrame == 2)
    assert(tracker.executedTransactionsInReverseOrder(u) == List())
    assertVersions(v,
      new Version(t, Set(x, z), pending = 0, changed = 0, Some(t)),
      new Version(t3, Set(x, z), pending = 0, changed = 0, None))
    assert(v.firstFrame == 2)
    assert(tracker.executedTransactionsInReverseOrder(v) == List())
    assertVersions(w,
      new Version(t, Set(z), pending = 0, changed = 0, Some(t)),
      new Version(t3, Set(z), pending = 0, changed = 0, Some(t3)))
    assert(w.firstFrame == 2)
    assert(tracker.executedTransactionsInReverseOrder(w) == List(t3))
    assertVersions(x,
      new Version(t, Set(y), pending = 0, changed = 0, Some(t)),
      new Version(t2, Set(y), pending = 0, changed = 0, None),
      new Version(t3, Set(y), pending = 0, changed = 0, None))
    assert(x.firstFrame == 3)
    assert(tracker.executedTransactionsInReverseOrder(x) == List())
    assertVersions(y,
      new Version(t, Set(z), pending = 0, changed = 0, Some(t)),
      new Version(t2, Set(z), pending = 0, changed = 0, None))
    assert(y.firstFrame == 2)
    assert(tracker.executedTransactionsInReverseOrder(y) == List())
    assertVersions(z,
      new Version(t, Set.empty, pending = 0, changed = 0, Some(t)),
      new Version(t2, Set.empty, pending = 0, changed = 0, None),
      new Version(t3, Set.empty, pending = 0, changed = 0, Some(t3)))
    assert(z.firstFrame == 3)
    assert(tracker.executedTransactionsInReverseOrder(z) == List(t3))
    assert(tracker.receivedInputs(z)(t3) == t)
  }
}
