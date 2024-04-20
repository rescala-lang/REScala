package tests.rescala.fullmv

import reactives.fullmv.FramingBranchResult.{Frame, FramingBranchEnd}
import reactives.fullmv.NotificationBranchResult.{DoNothing, ReevaluationReady}
import reactives.fullmv.NotificationBranchResult.ReevOutBranchResult.{NotifyAndReevaluationReadySuccessor, PureNotifyOnly}
import reactives.fullmv._

import scala.concurrent.duration.Duration

class NodeVersionHistoryTest extends munit.FunSuite {
  test("Frame Notify Reevout") {
    val engine = new FullMVEngine(Duration.Zero, "fnr")

    val createN = engine.newTurn()
    createN.beginExecuting()
    val n = new NonblockingSkipListVersionHistory[Int, FullMVTurn](createN, 10)
    createN.completeExecuting()

    val turn1 = engine.newTurn()
    turn1.beginFraming()
    assertEquals(n.incrementFrame(turn1), Frame(Set.empty, turn1))
    assertEquals(n.incrementFrame(turn1), FramingBranchEnd)
    turn1.completeFraming()
    assertEquals(n.notify(turn1, changed = true), true   -> DoNothing)
    assertEquals(n.notify(turn1, changed = false), false -> ReevaluationReady)
    assertEquals(n.reevIn(turn1), 10)
    assertEquals(n.reevOut(turn1, Some(5), identity), PureNotifyOnly(Set.empty))
    turn1.completeExecuting()

    val turn2 = engine.newTurn()
    turn2.beginFraming()
    assertEquals(n.incrementFrame(turn2), Frame(Set.empty, turn2))
    turn2.completeFraming()
    assertEquals(n.notify(turn2, changed = true), true -> ReevaluationReady)
    assertEquals(n.reevIn(turn2), 5)
    assertEquals(n.reevOut(turn2, Some(10), identity), PureNotifyOnly(Set.empty))
    turn2.completeExecuting()
  }

  test("Unchanged") {
    val engine = new FullMVEngine(Duration.Zero, "fnr")

    val createN = engine.newTurn()
    createN.beginExecuting()
    val n = new NonblockingSkipListVersionHistory[Int, FullMVTurn](createN, 10)
    createN.completeExecuting()

    val turn1 = engine.newTurn()
    turn1.beginFraming()
    assertEquals(n.incrementFrame(turn1), Frame(Set.empty, turn1))
    turn1.completeFraming()
    assertEquals(n.notify(turn1, changed = false), true -> PureNotifyOnly(Set.empty))
    turn1.completeExecuting()

    val turn2 = engine.newTurn()
    turn2.beginFraming()
    assertEquals(n.incrementFrame(turn2), Frame(Set.empty, turn2))
    turn2.completeFraming()

    val turn3 = engine.newTurn()
    turn3.beginFraming()
    assertEquals(n.incrementFrame(turn3), FramingBranchEnd)
    turn3.completeFraming()

    assertEquals(n.notify(turn3, changed = true), true  -> DoNothing)
    assertEquals(n.notify(turn2, changed = false), true -> NotifyAndReevaluationReadySuccessor(Set.empty, turn3))
    assertEquals(n.reevIn(turn3), 10)
    assertEquals(n.reevOut(turn3, Some(5), identity), PureNotifyOnly(Set.empty))
    turn2.completeExecuting()
  }

  test("SupersedeFraming into double marker trailer") {
    val engine = new FullMVEngine(Duration.Zero, "asd")

    val createN = engine.newTurn()
    createN.beginExecuting()
    val n = new NonblockingSkipListVersionHistory[Int, FullMVTurn](createN, 10)
    createN.completeExecuting()

    val reevaluate = engine.newTurn()
    reevaluate.beginFraming()
    assertEquals(n.incrementFrame(reevaluate), Frame(Set.empty, reevaluate))
    reevaluate.completeFraming()

    val framing1 = engine.newTurn()
    framing1.beginFraming()
    val framing2 = engine.newTurn()
    framing2.beginFraming()
    val lock = SerializationGraphTracking.tryLock(framing1, framing2, UnlockedUnknown) match {
      case x @ LockedSameSCC(_) => x
      case other                => fail("not locked: " + other)
    }
    framing2.addPredecessor(framing1.selfNode)
    lock.unlock()

    assertEquals(n.incrementFrame(framing2), FramingBranchEnd) // End because earlier frame by reevaluate turn exists

    n.notify(reevaluate, changed = true)
    n.retrofitSinkFrames(Nil, Some(framing1), -1)
    assertEquals(n.reevOut(reevaluate, Some(11), identity), PureNotifyOnly(Set.empty))
//    assertEquals(n.reevOut(reevaluate, Some(Pulse.Value(11))), FollowFraming(Set.empty, framing2))

    assertEquals(n.incrementSupersedeFrame(framing1, framing2), FramingBranchEnd)
//    assertEquals(n.incrementSupersedeFrame(framing1, framing2), Deframe(Set.empty, framing2))
  }
}
