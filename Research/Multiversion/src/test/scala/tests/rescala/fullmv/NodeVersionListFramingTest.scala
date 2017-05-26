//package tests.rescala.fullmv
//
//import org.scalatest.{FlatSpec, Matchers}
//import rescala.fullmv._
//
//class NodeVersionListFramingTest extends FlatSpec with Matchers {
//  import tests.rescala.fullmv.VersionListAsserter._
//
//  "A Version List" should "contain only the initial value version after initialization" in {
//    val tracker = new UserComputationTracker
//    val t = Transaction().start()
//    val x = new NodeVersionHistory(TestHost, t, t, tracker.comp)
//    t.done()
//    assertVersions(x,
//      new Version(t, Set.empty, 0, 0, Some(t)))
//  }
//
//  it should "record a version when being framed" in {
//    val tracker = new UserComputationTracker
//    val t = Transaction().start()
//    val x = new SignalVersionList(TestHost, t, t, tracker.comp)
//    t.done()
//    assert(x.firstFrame == 1)
//
//    val t2 = Transaction()
//    t2.branches(1)
//    x.incrementFrame(t2)
//    t2.start()
//    assertVersions(x,
//      new Version(t, Set.empty, pending = 0, changed = 0, Some(t)),
//      new Version(t2, Set.empty, pending = 1, changed = 0, None))
//    assert(x.firstFrame == 1)
//
//    val t3 = Transaction()
//    t3.branches(1)
//    x.incrementFrame(t3)
//    t3.start()
//    assertVersions(x,
//      new Version(t, Set.empty, pending = 0, changed = 0, Some(t)),
//      new Version(t2, Set.empty, pending = 1, changed = 0, None),
//      new Version(t3, Set.empty, pending = 1, changed = 0, None))
//    assert(x.firstFrame == 1)
//  }
//
//  it should "establish dependency edges" in {
//    val tracker = new UserComputationTracker
//    val t = Transaction().start()
//    val x = new SignalVersionList(TestHost, t, t, tracker.comp)
//    val y = new SignalVersionList(TestHost, t, t, tracker.comp)
//    x.discoverSuspend(ReevaluationTicket(t, y))
//    t.done()
//
//    assertVersions(x,
//      new Version(t, Set(y), pending = 0, changed = 0, Some(t)))
//    assertVersions(y,
//      new Version(t, Set.empty, pending = 0, changed = 0, Some(t)))
//  }
//
//  it should "propagate framing partially" in {
//    val tracker = new UserComputationTracker
//    val t = Transaction().start()
//    val x, y = new SignalVersionList(TestHost, t, t, tracker.comp)
//    x.discoverSuspend(ReevaluationTicket(t, y))
//    t.done()
//
//    val t2 = Transaction()
//    t2.branches(1)
//    x.incrementFrame(t2)
//    t2.start()
//    assertVersions(x,
//      new Version(t, Set(y), pending = 0, changed = 0, Some(t)),
//      new Version(t2, Set(y), pending = 1, changed = 0, None))
//    assert(x.firstFrame == 1)
//    assertVersions(y,
//      new Version(t, Set.empty, pending = 0, changed = 0, Some(t)),
//      new Version(t2, Set.empty, pending = 1, changed = 0, None))
//    assert(y.firstFrame == 1)
//
//    val t3 = Transaction()
//    t3.branches(1)
//    x.incrementFrame(t3)
//    t3.start()
//    assertVersions(x,
//      new Version(t, Set(y), pending = 0, changed = 0, Some(t)),
//      new Version(t2, Set(y), pending = 1, changed = 0, None),
//      new Version(t3, Set(y), pending = 1, changed = 0, None))
//    assert(x.firstFrame == 1)
//    assertVersions(y,
//      new Version(t, Set.empty, pending = 0, changed = 0, Some(t)),
//      new Version(t2, Set.empty, pending = 1, changed = 0, None))
//    assert(y.firstFrame == 1)
//  }
//
//  it should "count up pending during framing" in {
//    val tracker = new UserComputationTracker
//    val t = Transaction().start()
//    val x, y, z, q, r = new SignalVersionList(TestHost, t, t, tracker.comp)
//    x.discoverSuspend(ReevaluationTicket(t, q))
//    y.discoverSuspend(ReevaluationTicket(t, q))
//    z.discoverSuspend(ReevaluationTicket(t, q))
//    q.discoverSuspend(ReevaluationTicket(t, r))
//    x.discoverSuspend(ReevaluationTicket(t, r))
//    t.done()
//
//    val t2 = Transaction()
//    t2.branches(3)
//    x.incrementFrame(t2)
//    y.incrementFrame(t2)
//    z.incrementFrame(t2)
//    t2.start()
//    assertVersions(q,
//      new Version(t, Set(r), pending = 0, changed = 0, Some(t)),
//      new Version(t2, Set(r), pending = 3, changed = 0, None))
//    assert(q.firstFrame == 1)
//    assertVersions(r,
//      new Version(t, Set.empty, pending = 0, changed = 0, Some(t)),
//      new Version(t2, Set.empty, pending = 2, changed = 0, None))
//    assert(r.firstFrame == 1)
//  }
//
//  it should "retroactively correct partial framing upon ill-ordered framings" in {
//    val tracker = new UserComputationTracker
//    val t = Transaction().start()
//    val x, y, z = new SignalVersionList(TestHost, t, t, tracker.comp)
//    x.discoverSuspend(ReevaluationTicket(t, y))
//    t.done()
//
//    val t2 = Transaction()
//    t2.branches(1)
//    z.incrementFrame(t2)
//
//    val t3 = Transaction()
//    t3.branches(1)
//    z.incrementFrame(t3)
//
//    val t4 = Transaction()
//    t4.branches(1)
//    z.incrementFrame(t4)
//
//    t4.branches(1)
//    x.incrementFrame(t4)
//
//    assertVersions(x,
//      new Version(t, Set(y), pending = 0, changed = 0, Some(t)),
//      new Version(t4, Set(y), pending = 1, changed = 0, None))
//    assert(x.firstFrame == 1)
//    assertVersions(y,
//      new Version(t, Set.empty, pending = 0, changed = 0, Some(t)),
//      new Version(t4, Set.empty, pending = 1, changed = 0, None))
//    println(y.firstFrame)
//    assert(y.firstFrame == 1)
//
//    t3.branches(1)
//    x.incrementFrame(t3)
//
//    assertVersions(x,
//      new Version(t, Set(y), pending = 0, changed = 0, Some(t)),
//      new Version(t3, Set(y), pending = 1, changed = 0, None),
//      new Version(t4, Set(y), pending = 1, changed = 0, None))
//    assertVersions(y,
//      new Version(t, Set.empty, pending = 0, changed = 0, Some(t)),
//      new Version(t3, Set.empty, pending = 1, changed = 0, None),
//      new Version(t4, Set.empty, pending = 0, changed = 0, None))
//
//    t2.branches(1)
//    x.incrementFrame(t2)
//
//    assertVersions(x,
//      new Version(t, Set(y), pending = 0, changed = 0, Some(t)),
//      new Version(t2, Set(y), pending = 1, changed = 0, None),
//      new Version(t3, Set(y), pending = 1, changed = 0, None),
//      new Version(t4, Set(y), pending = 1, changed = 0, None))
//    assertVersions(y,
//      new Version(t, Set.empty, pending = 0, changed = 0, Some(t)),
//      new Version(t2, Set.empty, pending = 1, changed = 0, None),
//      new Version(t3, Set.empty, pending = 0, changed = 0, None),
//      new Version(t4, Set.empty, pending = 0, changed = 0, None))
//
//    t2.start()
//    t3.start()
//    t4.start()
//  }
//}
