package tests.rescala.fullmv

import org.scalatest.{FlatSpec, Matchers}
import rescala.fullmv.api._

class UserComputationTracker {
  var inputs = Map[Transaction, Transaction]()
  var sequence = Seq[Transaction]()
  val comp = {(txn: Transaction, v_in: Transaction) =>
    inputs += txn -> v_in
    txn
  }
}

class VersionListTest extends FlatSpec with Matchers {
  import VersionListAsserter._
  object host extends Host {
    override val sgt: SerializationGraphTracking = GlobalLockSGT
    override val taskPool: TaskPool = ExecuteImmediatelyTaskPool
  }

  "A Version List" should "contain only the initial value version after initialization" in {
    val tracker = new UserComputationTracker
    val t = Transaction().start()
    val x = new SignalVersionList(host, t, t, tracker.comp)
    assertVersions(x,
      new Version(t, Set.empty, 0, 0, Some(t)))
  }

  it should "record a version when being framed" in {
    val tracker = new UserComputationTracker
    val t = Transaction().start()
    val x = new SignalVersionList(host, t, t, tracker.comp)

    val t2 = Transaction()
    t2.branches(1)
    x.incrementFrame(t2)

    val t3 = Transaction()
    t3.branches(1)
    x.incrementFrame(t3)

    assertVersions(x,
      new Version(t, Set.empty, pending = 0, changed = 0, Some(t)),
      new Version(t2, Set.empty, pending = 1, changed = 0, None),
      new Version(t3, Set.empty, pending = 1, changed = 0, None))
  }
}
