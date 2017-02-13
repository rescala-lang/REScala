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

    x.notify(t3, true, None)
    assertVersions(x,
      new Version(t, Set.empty, pending = 0, changed = 0, Some(t)),
      new Version(t2, Set.empty, pending = 1, changed = 0, None),
      new Version(t3, Set.empty, pending = 0, changed = 1, None),
      new Version(t4, Set.empty, pending = 1, changed = 0, None))
    assert(x.firstFrame == 1)

    x.notify(t2, true, None)
    assertVersions(x,
      new Version(t, Set.empty, pending = 0, changed = 0, Some(t)),
      new Version(t2, Set.empty, pending = 0, changed = 0, Some(t2)),
      new Version(t3, Set.empty, pending = 0, changed = 0, Some(t3)),
      new Version(t4, Set.empty, pending = 1, changed = 0, None))
    assert(x.firstFrame == 3)

    x.notify(t4, false, None)
    assertVersions(x,
      new Version(t, Set.empty, pending = 0, changed = 0, Some(t)),
      new Version(t2, Set.empty, pending = 0, changed = 0, Some(t2)),
      new Version(t3, Set.empty, pending = 0, changed = 0, Some(t3)),
      new Version(t4, Set.empty, pending = 0, changed = 0, None))
    assert(x.firstFrame == 4)
  }
}
