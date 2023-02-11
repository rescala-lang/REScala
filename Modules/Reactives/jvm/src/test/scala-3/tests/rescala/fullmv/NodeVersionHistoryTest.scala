package tests.rescala.fullmv

import org.scalatest.funsuite.AnyFunSuite
import rescala.fullmv.FramingBranchResult.{Frame, FramingBranchEnd}
import rescala.fullmv.NotificationBranchResult.{DoNothing, ReevaluationReady}
import rescala.fullmv.NotificationBranchResult.ReevOutBranchResult.{NotifyAndReevaluationReadySuccessor, PureNotifyOnly}
import rescala.fullmv._

import scala.concurrent.duration.Duration

class NodeVersionHistoryTest extends AnyFunSuite {
  test("Frame Notify Reevout") {
    val api    = new FullMVApi(Duration.Zero, "fnr")
    val engine = api.scheduler
    import api._

    val createN = engine.newTurn()
    createN.beginExecuting()
    val n = new NonblockingSkipListVersionHistory[Int, FullMVTurn](createN, 10)
    createN.completeExecuting()

    val turn1 = engine.newTurn()
    turn1.beginFraming()
    assert(n.incrementFrame(turn1) === Frame(Set.empty, turn1))
    assert(n.incrementFrame(turn1) === FramingBranchEnd)
    turn1.completeFraming()
    assert(n.notify(turn1, changed = true) === true   -> DoNothing)
    assert(n.notify(turn1, changed = false) === false -> ReevaluationReady)
    assert(n.reevIn(turn1) === 10)
    assert(n.reevOut(turn1, Some(5), identity) === PureNotifyOnly(Set.empty))
    turn1.completeExecuting()

    val turn2 = engine.newTurn()
    turn2.beginFraming()
    assert(n.incrementFrame(turn2) === Frame(Set.empty, turn2))
    turn2.completeFraming()
    assert(n.notify(turn2, changed = true) === true -> ReevaluationReady)
    assert(n.reevIn(turn2) === 5)
    assert(n.reevOut(turn2, Some(10), identity) === PureNotifyOnly(Set.empty))
    turn2.completeExecuting()
  }

  test("Unchanged") {
    object api extends FullMVApi(Duration.Zero, "nochange")
    val engine = api.scheduler
    import api._

    val createN = engine.newTurn()
    createN.beginExecuting()
    val n = new NonblockingSkipListVersionHistory[Int, FullMVTurn](createN, 10)
    createN.completeExecuting()

    val turn1 = engine.newTurn()
    turn1.beginFraming()
    assert(n.incrementFrame(turn1) === Frame(Set.empty, turn1))
    turn1.completeFraming()
    assert(n.notify(turn1, changed = false) === true -> PureNotifyOnly(Set.empty))
    turn1.completeExecuting()

    val turn2 = engine.newTurn()
    turn2.beginFraming()
    assert(n.incrementFrame(turn2) === Frame(Set.empty, turn2))
    turn2.completeFraming()

    val turn3 = engine.newTurn()
    turn3.beginFraming()
    assert(n.incrementFrame(turn3) === FramingBranchEnd)
    turn3.completeFraming()

    assert(n.notify(turn3, changed = true) === true  -> DoNothing)
    assert(n.notify(turn2, changed = false) === true -> NotifyAndReevaluationReadySuccessor(Set.empty, turn3))
    assert(n.reevIn(turn3) === 10)
    assert(n.reevOut(turn3, Some(5), identity) === PureNotifyOnly(Set.empty))
    turn2.completeExecuting()
  }

  test("SupersedeFraming into double marker trailer") {
    object api extends FullMVApi(Duration.Zero, "asd")
    val engine = api.scheduler
    import api._

    val createN = engine.newTurn()
    createN.beginExecuting()
    val n = new NonblockingSkipListVersionHistory[Int, FullMVTurn](createN, 10)
    createN.completeExecuting()

    val reevaluate = engine.newTurn()
    reevaluate.beginFraming()
    assert(n.incrementFrame(reevaluate) === Frame(Set.empty, reevaluate))
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

    assert(n.incrementFrame(framing2) === FramingBranchEnd) // End because earlier frame by reevaluate turn exists

    n.notify(reevaluate, changed = true)
    n.retrofitSinkFrames(Nil, Some(framing1), -1)
    assert(n.reevOut(reevaluate, Some(11), identity) === PureNotifyOnly(Set.empty))
//    assert(n.reevOut(reevaluate, Some(Pulse.Value(11))) === FollowFraming(Set.empty, framing2))

    assert(n.incrementSupersedeFrame(framing1, framing2) === FramingBranchEnd)
//    assert(n.incrementSupersedeFrame(framing1, framing2) === Deframe(Set.empty, framing2))
  }
}
