package tests.rescala.fullmv

import org.scalatest.{FlatSpec, Matchers}
import rescala.fullmv.api._
import tests.rescala.fullmv.testutils.{TestHost, UserComputationTracker}



class NodeVersionListFramingTest extends FlatSpec with Matchers {
  import tests.rescala.fullmv.testutils.VersionListAsserter._

  "A Version List" should "contain only the initial value version after initialization" in {
    val tracker = new UserComputationTracker
    val t = Transaction().start()
    val x = new SignalVersionList(TestHost, t, t, tracker.comp)
    assertVersions(x,
      new Version(t, Set.empty, 0, 0, Some(t)))
  }

  it should "record a version when being framed" in {
    val tracker = new UserComputationTracker
    val t = Transaction().start()
    val x = new SignalVersionList(TestHost, t, t, tracker.comp)
    assert(x.firstFrame == 1)

    val t2 = Transaction()
    t2.branches(1)
    x.incrementFrame(t2)
    assertVersions(x,
      new Version(t, Set.empty, pending = 0, changed = 0, Some(t)),
      new Version(t2, Set.empty, pending = 1, changed = 0, None))
    assert(x.firstFrame == 1)

    val t3 = Transaction()
    t3.branches(1)
    x.incrementFrame(t3)
    assertVersions(x,
      new Version(t, Set.empty, pending = 0, changed = 0, Some(t)),
      new Version(t2, Set.empty, pending = 1, changed = 0, None),
      new Version(t3, Set.empty, pending = 1, changed = 0, None))
    assert(x.firstFrame == 1)
  }

  it should "establish dependency edges" in {
    val trackerX, trackerY = new UserComputationTracker
    val t = Transaction().start()
    val x = new SignalVersionList(TestHost, t, t, trackerX.comp)
    val y = new SignalVersionList(TestHost, t, t, trackerY.comp)
    x.discoverSuspend(t, y)

    assertVersions(x,
      new Version(t, Set(y), pending = 0, changed = 0, Some(t)))
    assertVersions(y,
      new Version(t, Set.empty, pending = 0, changed = 0, Some(t)))
  }

  it should "propagate framing partially" in {
    val trackerX, trackerY = new UserComputationTracker
    val t = Transaction().start()
    val x = new SignalVersionList(TestHost, t, t, trackerX.comp)
    val y = new SignalVersionList(TestHost, t, t, trackerY.comp)
    x.discoverSuspend(t, y)

    val t2 = Transaction()
    t2.branches(1)
    x.incrementFrame(t2)
    assertVersions(x,
      new Version(t, Set(y), pending = 0, changed = 0, Some(t)),
      new Version(t2, Set(y), pending = 1, changed = 0, None))
    assert(x.firstFrame == 1)
    assertVersions(y,
      new Version(t, Set.empty, pending = 0, changed = 0, Some(t)),
      new Version(t2, Set.empty, pending = 1, changed = 0, None))
    assert(y.firstFrame == 1)

    val t3 = Transaction()
    t3.branches(1)
    x.incrementFrame(t3)
    assertVersions(x,
      new Version(t, Set(y), pending = 0, changed = 0, Some(t)),
      new Version(t2, Set(y), pending = 1, changed = 0, None),
      new Version(t3, Set(y), pending = 1, changed = 0, None))
    assert(x.firstFrame == 1)
    assertVersions(y,
      new Version(t, Set.empty, pending = 0, changed = 0, Some(t)),
      new Version(t2, Set.empty, pending = 1, changed = 0, None))
    assert(y.firstFrame == 1)
  }

  it should "count up pending during framing" in {
    val trackerX, trackerY, trackerZ, trackerQ, trackerR = new UserComputationTracker
    val t = Transaction().start()
    val x = new SignalVersionList(TestHost, t, t, trackerX.comp)
    val y = new SignalVersionList(TestHost, t, t, trackerY.comp)
    val z = new SignalVersionList(TestHost, t, t, trackerZ.comp)
    val q = new SignalVersionList(TestHost, t, t, trackerQ.comp)
    val r = new SignalVersionList(TestHost, t, t, trackerR.comp)
    x.discoverSuspend(t, q)
    y.discoverSuspend(t, q)
    z.discoverSuspend(t, q)
    q.discoverSuspend(t, r)
    x.discoverSuspend(t, r)

    val t2 = Transaction()
    t2.branches(3)
    x.incrementFrame(t2)
    y.incrementFrame(t2)
    z.incrementFrame(t2)
    assertVersions(q,
      new Version(t, Set(r), pending = 0, changed = 0, Some(t)),
      new Version(t2, Set(r), pending = 3, changed = 0, None))
    assert(q.firstFrame == 1)
    assertVersions(r,
      new Version(t, Set.empty, pending = 0, changed = 0, Some(t)),
      new Version(t2, Set.empty, pending = 2, changed = 0, None))
    assert(r.firstFrame == 1)
  }

  it should "retroactively correct partial framing upon ill-ordered framings" in {
    val trackerX, trackerY, trackerZ = new UserComputationTracker
    val t = Transaction().start()
    val x = new SignalVersionList(TestHost, t, t, trackerX.comp)
    val y = new SignalVersionList(TestHost, t, t, trackerY.comp)
    val z = new SignalVersionList(TestHost, t, t, trackerZ.comp)
    x.discoverSuspend(t, y)

    val t2 = Transaction()
    t2.branches(1)
    z.incrementFrame(t2)

    val t3 = Transaction()
    t3.branches(1)
    z.incrementFrame(t3)

    val t4 = Transaction()
    t4.branches(1)
    z.incrementFrame(t4)

    t4.branches(1)
    x.incrementFrame(t4)

    assertVersions(x,
      new Version(t, Set(y), pending = 0, changed = 0, Some(t)),
      new Version(t4, Set(y), pending = 1, changed = 0, None))
    assert(x.firstFrame == 1)
    assertVersions(y,
      new Version(t, Set.empty, pending = 0, changed = 0, Some(t)),
      new Version(t4, Set.empty, pending = 1, changed = 0, None))
    println(y.firstFrame)
    assert(y.firstFrame == 1)

    t3.branches(1)
    x.incrementFrame(t3)

    assertVersions(x,
      new Version(t, Set(y), pending = 0, changed = 0, Some(t)),
      new Version(t3, Set(y), pending = 1, changed = 0, None),
      new Version(t4, Set(y), pending = 1, changed = 0, None))
    assertVersions(y,
      new Version(t, Set.empty, pending = 0, changed = 0, Some(t)),
      new Version(t3, Set.empty, pending = 1, changed = 0, None),
      new Version(t4, Set.empty, pending = 0, changed = 0, None))

    t2.branches(1)
    x.incrementFrame(t2)

    assertVersions(x,
      new Version(t, Set(y), pending = 0, changed = 0, Some(t)),
      new Version(t2, Set(y), pending = 1, changed = 0, None),
      new Version(t3, Set(y), pending = 1, changed = 0, None),
      new Version(t4, Set(y), pending = 1, changed = 0, None))
    assertVersions(y,
      new Version(t, Set.empty, pending = 0, changed = 0, Some(t)),
      new Version(t2, Set.empty, pending = 1, changed = 0, None),
      new Version(t3, Set.empty, pending = 0, changed = 0, None),
      new Version(t4, Set.empty, pending = 0, changed = 0, None))
  }
}
