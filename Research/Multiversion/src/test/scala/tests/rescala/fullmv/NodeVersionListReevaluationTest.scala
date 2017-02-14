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
    assert(tracker.executedTransactionsInReverseOrder == List())

    x.notify(t3, true, None)
    assertVersions(x,
      new Version(t, Set.empty, pending = 0, changed = 0, Some(t)),
      new Version(t2, Set.empty, pending = 1, changed = 0, None),
      new Version(t3, Set.empty, pending = 0, changed = 1, None),
      new Version(t4, Set.empty, pending = 1, changed = 0, None))
    assert(x.firstFrame == 1)
    assert(tracker.executedTransactionsInReverseOrder == List())

    x.notify(t2, true, None)
    assertVersions(x,
      new Version(t, Set.empty, pending = 0, changed = 0, Some(t)),
      new Version(t2, Set.empty, pending = 0, changed = 0, Some(t2)),
      new Version(t3, Set.empty, pending = 0, changed = 0, Some(t3)),
      new Version(t4, Set.empty, pending = 1, changed = 0, None))
    assert(x.firstFrame == 3)
    assert(tracker.executedTransactionsInReverseOrder == List(t3, t2))
    assert(tracker.receivedInputs(t2) == t)
    assert(tracker.receivedInputs(t3) == t2)

    x.notify(t4, false, None)
    assertVersions(x,
      new Version(t, Set.empty, pending = 0, changed = 0, Some(t)),
      new Version(t2, Set.empty, pending = 0, changed = 0, Some(t2)),
      new Version(t3, Set.empty, pending = 0, changed = 0, Some(t3)),
      new Version(t4, Set.empty, pending = 0, changed = 0, None))
    assert(x.firstFrame == 4)
    assert(tracker.executedTransactionsInReverseOrder == List(t3, t2))
  }

  it should "propagate reevaluations" in {
    val trackerX, trackerY, trackerZ = new UserComputationTracker
    val t = Transaction().start()
    val x = new SignalVersionList(TestHost, t, t, trackerX.comp)
    val y = new SignalVersionList(TestHost, t, t, trackerY.comp)
    x.discoverSuspend(t, y)
    val z = new SignalVersionList(TestHost, t, t, trackerZ.comp)
    y.discoverSuspend(t, z)

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
    assert(trackerX.executedTransactionsInReverseOrder == List())
    assertVersions(y,
      new Version(t, Set(z), pending = 0, changed = 0, Some(t)),
      new Version(t2, Set(z), pending = 1, changed = 0, None))
    assert(y.firstFrame == 1)
    assert(trackerY.executedTransactionsInReverseOrder == List())
    assertVersions(z,
      new Version(t, Set.empty, pending = 0, changed = 0, Some(t)),
      new Version(t2, Set.empty, pending = 1, changed = 0, None))
    assert(z.firstFrame == 1)
    assert(trackerZ.executedTransactionsInReverseOrder == List())

    t2.branches(1)
    x.notify(t2, true, None)

    assertVersions(x,
      new Version(t, Set(y), pending = 0, changed = 0, Some(t)),
      new Version(t2, Set(y), pending = 0, changed = 0, Some(t2)),
      new Version(t3, Set(y), pending = 0, changed = 0, Some(t3)))
    assert(x.firstFrame == 3)
    assert(trackerX.executedTransactionsInReverseOrder == List(t3, t2))
    assert(trackerX.receivedInputs(t2) == t)
    assert(trackerX.receivedInputs(t3) == t2)
    assertVersions(y,
      new Version(t, Set(z), pending = 0, changed = 0, Some(t)),
      new Version(t2, Set(z), pending = 0, changed = 0, Some(t2)),
      new Version(t3, Set(z), pending = 0, changed = 0, Some(t3)))
    assert(y.firstFrame == 3)
    assert(trackerY.executedTransactionsInReverseOrder == List(t3, t2))
    assert(trackerY.receivedInputs(t2) == t)
    assert(trackerY.receivedInputs(t3) == t2)
    assertVersions(z,
      new Version(t, Set.empty, pending = 0, changed = 0, Some(t)),
      new Version(t2, Set.empty, pending = 0, changed = 0, Some(t2)),
      new Version(t3, Set.empty, pending = 0, changed = 0, Some(t3)))
    assert(z.firstFrame == 3)
    assert(trackerZ.executedTransactionsInReverseOrder == List(t3, t2))
    assert(trackerZ.receivedInputs(t2) == t)
    assert(trackerZ.receivedInputs(t3) == t2)

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
    assert(trackerX.executedTransactionsInReverseOrder == List(t3, t2))
    assertVersions(y,
      new Version(t, Set(z), pending = 0, changed = 0, Some(t)),
      new Version(t2, Set(z), pending = 0, changed = 0, Some(t2)),
      new Version(t3, Set(z), pending = 0, changed = 0, Some(t3)),
      new Version(t4, Set(z), pending = 1, changed = 0, None))
    assert(y.firstFrame == 3)
    assert(trackerY.executedTransactionsInReverseOrder == List(t3, t2))
    assertVersions(z,
      new Version(t, Set.empty, pending = 0, changed = 0, Some(t)),
      new Version(t2, Set.empty, pending = 0, changed = 0, Some(t2)),
      new Version(t3, Set.empty, pending = 0, changed = 0, Some(t3)),
      new Version(t4, Set.empty, pending = 1, changed = 0, None))
    assert(z.firstFrame == 3)
    assert(trackerZ.executedTransactionsInReverseOrder == List(t3, t2))

    t4.branches(1)
    x.notify(t4, true, None)

    assertVersions(x,
      new Version(t, Set(y), pending = 0, changed = 0, Some(t)),
      new Version(t2, Set(y), pending = 0, changed = 0, Some(t2)),
      new Version(t3, Set(y), pending = 0, changed = 0, Some(t3)),
      new Version(t4, Set(y), pending = 0, changed = 0, Some(t4)))
    assert(x.firstFrame == 4)
    assert(trackerX.executedTransactionsInReverseOrder == List(t4, t3, t2))
    assert(trackerX.receivedInputs(t4) == t3)
    assertVersions(y,
      new Version(t, Set(z), pending = 0, changed = 0, Some(t)),
      new Version(t2, Set(z), pending = 0, changed = 0, Some(t2)),
      new Version(t3, Set(z), pending = 0, changed = 0, Some(t3)),
      new Version(t4, Set(z), pending = 0, changed = 0, Some(t4)))
    assert(y.firstFrame == 4)
    assert(trackerY.executedTransactionsInReverseOrder == List(t4, t3, t2))
    assert(trackerY.receivedInputs(t4) == t3)
    assertVersions(z,
      new Version(t, Set.empty, pending = 0, changed = 0, Some(t)),
      new Version(t2, Set.empty, pending = 0, changed = 0, Some(t2)),
      new Version(t3, Set.empty, pending = 0, changed = 0, Some(t3)),
      new Version(t4, Set.empty, pending = 0, changed = 0, Some(t4)))
    assert(z.firstFrame == 4)
    assert(trackerZ.executedTransactionsInReverseOrder == List(t4, t3, t2))
    assert(trackerZ.receivedInputs(t4) == t3)
  }

  it should "propagate partial framings with reevaluations" in {
    val trackerW, trackerX, trackerY, trackerZ, trackerQ = new UserComputationTracker
    val t = Transaction().start()
    val w = new SignalVersionList(TestHost, t, t, trackerW.comp)
    val x = new SignalVersionList(TestHost, t, t, trackerX.comp)
    w.discoverSuspend(t, x)
    val y = new SignalVersionList(TestHost, t, t, trackerY.comp)
    x.discoverSuspend(t, y)
    val z = new SignalVersionList(TestHost, t, t, trackerZ.comp)
    y.discoverSuspend(t, z)
    val q = new SignalVersionList(TestHost, t, t, trackerQ.comp)

    val t2 = Transaction()
    t2.branches(1)
    w.incrementFrame(t2)
    t2.start()

    val t3 = Transaction()
    t3.branches(2)
    w.incrementFrame(t3)
    q.incrementFrame(t3)
    t3.start()

    val t4 = Transaction()
    t4.branches(2)
    y.incrementFrame(t4)
    q.incrementFrame(t4)
    t4.start()

    assertVersions(w,
      new Version(t, Set(x), pending = 0, changed = 0, Some(t)),
      new Version(t2, Set(x), pending = 1, changed = 0, None),
      new Version(t3, Set(x), pending = 1, changed = 0, None))
    assert(w.firstFrame == 1)
    assert(trackerW.executedTransactionsInReverseOrder == List())
    assertVersions(x,
      new Version(t, Set(y), pending = 0, changed = 0, Some(t)),
      new Version(t2, Set(y), pending = 1, changed = 0, None))
    assert(x.firstFrame == 1)
    assert(trackerX.executedTransactionsInReverseOrder == List())
    assertVersions(y,
      new Version(t, Set(z), pending = 0, changed = 0, Some(t)),
      new Version(t2, Set(z), pending = 1, changed = 0, None),
      new Version(t4, Set(z), pending = 1, changed = 0, None))
    assert(y.firstFrame == 1)
    assert(trackerY.executedTransactionsInReverseOrder == List())
    assertVersions(z,
      new Version(t, Set.empty, pending = 0, changed = 0, Some(t)),
      new Version(t2, Set.empty, pending = 1, changed = 0, None))
    assert(z.firstFrame == 1)
    assert(trackerZ.executedTransactionsInReverseOrder == List())

    t2.branches(1)
    w.notify(t2, true, None)

    assertVersions(w,
      new Version(t, Set(x), pending = 0, changed = 0, Some(t)),
      new Version(t2, Set(x), pending = 0, changed = 0, Some(t2)),
      new Version(t3, Set(x), pending = 1, changed = 0, None))
    assert(w.firstFrame == 2)
    assert(trackerW.executedTransactionsInReverseOrder == List(t2))
    assert(trackerW.receivedInputs(t2) == t)
    assertVersions(x,
      new Version(t, Set(y), pending = 0, changed = 0, Some(t)),
      new Version(t2, Set(y), pending = 0, changed = 0, Some(t2)),
      new Version(t3, Set(y), pending = 1, changed = 0, None))
    assert(x.firstFrame == 2)
    assert(trackerX.executedTransactionsInReverseOrder == List(t2))
    assert(trackerX.receivedInputs(t2) == t)
    assertVersions(y,
      new Version(t, Set(z), pending = 0, changed = 0, Some(t)),
      new Version(t2, Set(z), pending = 0, changed = 0, Some(t2)),
      new Version(t3, Set(z), pending = 1, changed = 0, None),
      new Version(t4, Set(z), pending = 1, changed = 0, None))
    assert(y.firstFrame == 2)
    assert(trackerY.executedTransactionsInReverseOrder == List(t2))
    assert(trackerY.receivedInputs(t2) == t)
    assertVersions(z,
      new Version(t, Set.empty, pending = 0, changed = 0, Some(t)),
      new Version(t2, Set.empty, pending = 0, changed = 0, Some(t2)),
      new Version(t3, Set.empty, pending = 1, changed = 0, None))
    assert(z.firstFrame == 2)
    assert(trackerZ.executedTransactionsInReverseOrder == List(t2))
    assert(trackerZ.receivedInputs(t2) == t)
  }

  it should "skip some framing/nochanges after a nochange catching up with partial framing" in {
    //    z
    //   / \
    //  y  |
    //  |  |
    //  x  |
    // / \/\
    // u v w
    val trackerU, trackerV, trackerW, trackerX, trackerY, trackerZ = new UserComputationTracker
    val t = Transaction().start()
    val u = new SignalVersionList(TestHost, t, t, trackerU.comp)
    val v = new SignalVersionList(TestHost, t, t, trackerV.comp)
    val w = new SignalVersionList(TestHost, t, t, trackerW.comp)
    val x = new SignalVersionList(TestHost, t, t, trackerX.comp)
    u.discoverSuspend(t, x)
    v.discoverSuspend(t, x)
    val y = new SignalVersionList(TestHost, t, t, trackerY.comp)
    x.discoverSuspend(t, y)
    val z = new SignalVersionList(TestHost, t, t, trackerZ.comp)
    v.discoverSuspend(t, z)
    y.discoverSuspend(t, z)
    w.discoverSuspend(t, z)

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
    assert(trackerU.executedTransactionsInReverseOrder == List())
    assertVersions(v,
      new Version(t, Set(x, z), pending = 0, changed = 0, Some(t)),
      new Version(t3, Set(x, z), pending = 1, changed = 0, None))
    assert(v.firstFrame == 1)
    assert(trackerV.executedTransactionsInReverseOrder == List())
    assertVersions(w,
      new Version(t, Set(z), pending = 0, changed = 0, Some(t)),
      new Version(t3, Set(z), pending = 1, changed = 0, None))
    assert(w.firstFrame == 1)
    assert(trackerW.executedTransactionsInReverseOrder == List())
    assertVersions(x,
      new Version(t, Set(y), pending = 0, changed = 0, Some(t)),
      new Version(t2, Set(y), pending = 1, changed = 0, None),
      new Version(t3, Set(y), pending = 1, changed = 0, None))
    assert(x.firstFrame == 1)
    assert(trackerX.executedTransactionsInReverseOrder == List())
    assertVersions(y,
      new Version(t, Set(z), pending = 0, changed = 0, Some(t)),
      new Version(t2, Set(z), pending = 1, changed = 0, None))
    assert(y.firstFrame == 1)
    assert(trackerY.executedTransactionsInReverseOrder == List())
    assertVersions(z,
      new Version(t, Set.empty, pending = 0, changed = 0, Some(t)),
      new Version(t2, Set.empty, pending = 1, changed = 0, None),
      new Version(t3, Set.empty, pending = 2, changed = 0, None))
    assert(z.firstFrame == 1)
    assert(trackerZ.executedTransactionsInReverseOrder == List())

    t3.branches(2)
    v.notify(t3, false, None)
    w.notify(t3, true, None)

    assertVersions(u,
      new Version(t, Set(x), pending = 0, changed = 0, Some(t)),
      new Version(t2, Set(x), pending = 1, changed = 0, None))
    assert(u.firstFrame == 1)
    assert(trackerU.executedTransactionsInReverseOrder == List())
    assertVersions(v,
      new Version(t, Set(x, z), pending = 0, changed = 0, Some(t)),
      new Version(t3, Set(x, z), pending = 0, changed = 0, None))
    assert(v.firstFrame == 2)
    assert(trackerV.executedTransactionsInReverseOrder == List())
    assertVersions(w,
      new Version(t, Set(z), pending = 0, changed = 0, Some(t)),
      new Version(t3, Set(z), pending = 0, changed = 0, Some(t3)))
    assert(w.firstFrame == 2)
    assert(trackerW.executedTransactionsInReverseOrder == List(t3))
    assert(trackerW.receivedInputs(t3) == t)
    assertVersions(x,
      new Version(t, Set(y), pending = 0, changed = 0, Some(t)),
      new Version(t2, Set(y), pending = 1, changed = 0, None),
      new Version(t3, Set(y), pending = 0, changed = 0, None))
    assert(x.firstFrame == 1)
    assert(trackerX.executedTransactionsInReverseOrder == List())
    assertVersions(y,
      new Version(t, Set(z), pending = 0, changed = 0, Some(t)),
      new Version(t2, Set(z), pending = 1, changed = 0, None))
    assert(y.firstFrame == 1)
    assert(trackerY.executedTransactionsInReverseOrder == List())
    assertVersions(z,
      new Version(t, Set.empty, pending = 0, changed = 0, Some(t)),
      new Version(t2, Set.empty, pending = 1, changed = 0, None),
      new Version(t3, Set.empty, pending = 0, changed = 1, None))
    assert(z.firstFrame == 1)
    assert(trackerZ.executedTransactionsInReverseOrder == List())

    t2.branches(1)
    u.notify(t2, false, None) // changed=true would work equally

    assertVersions(u,
      new Version(t, Set(x), pending = 0, changed = 0, Some(t)),
      new Version(t2, Set(x), pending = 0, changed = 0, None))
    assert(u.firstFrame == 2)
    assert(trackerU.executedTransactionsInReverseOrder == List())
    assertVersions(v,
      new Version(t, Set(x, z), pending = 0, changed = 0, Some(t)),
      new Version(t3, Set(x, z), pending = 0, changed = 0, None))
    assert(v.firstFrame == 2)
    assert(trackerV.executedTransactionsInReverseOrder == List())
    assertVersions(w,
      new Version(t, Set(z), pending = 0, changed = 0, Some(t)),
      new Version(t3, Set(z), pending = 0, changed = 0, Some(t3)))
    assert(w.firstFrame == 2)
    assert(trackerW.executedTransactionsInReverseOrder == List(t3))
    assert(trackerW.receivedInputs(t3) == t)
    assertVersions(x,
      new Version(t, Set(y), pending = 0, changed = 0, Some(t)),
      new Version(t2, Set(y), pending = 0, changed = 0, None),
      new Version(t3, Set(y), pending = 0, changed = 0, None))
    assert(x.firstFrame == 3)
    assert(trackerX.executedTransactionsInReverseOrder == List())
    assertVersions(y,
      new Version(t, Set(z), pending = 0, changed = 0, Some(t)),
      new Version(t2, Set(z), pending = 0, changed = 0, None))
    assert(y.firstFrame == 2)
    assert(trackerY.executedTransactionsInReverseOrder == List())
    assertVersions(z,
      new Version(t, Set.empty, pending = 0, changed = 0, Some(t)),
      new Version(t2, Set.empty, pending = 0, changed = 0, None),
      new Version(t3, Set.empty, pending = 0, changed = 0, Some(t3)))
    assert(z.firstFrame == 3)
    assert(trackerZ.executedTransactionsInReverseOrder == List(t3))
    assert(trackerZ.receivedInputs(t3) == t)
  }

  ignore should "ensure glitch freedom" in {

  }

  ignore should "ensure glitch freedom on nodes skipped by partial framing" in {

  }
}
