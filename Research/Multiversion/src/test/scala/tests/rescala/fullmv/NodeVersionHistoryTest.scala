package tests.rescala.fullmv

import org.scalatest.FunSuite
import rescala.core.{Pulse, ValuePersistency}
import rescala.fullmv.FramingBranchResult.{Deframe, Frame, FramingBranchEnd}
import rescala.fullmv.NotificationResultAction.NotificationOutAndSuccessorOperation.FollowFraming
import rescala.fullmv._

import scala.concurrent.duration.Duration

class NodeVersionHistoryTest extends FunSuite {
  test("SupersedeFraming into double marker trailer") {
    val engine = new FullMVEngine(Duration.Zero, "asd")

    val createN = engine.newTurn()
    createN.awaitAndSwitchPhase(TurnPhase.Executing)
    val n = new NodeVersionHistory[Pulse[Int], FullMVTurn, Int, Int](createN, ValuePersistency.InitializedSignal(Pulse.Value(10)))
    createN.awaitAndSwitchPhase(TurnPhase.Completed)

    val reevaluate = engine.newTurn()
    reevaluate.awaitAndSwitchPhase(TurnPhase.Framing)
    assert(n.incrementFrame(reevaluate) === Frame(Set.empty, reevaluate))
    reevaluate.awaitAndSwitchPhase(TurnPhase.Executing)

    val framing1 = engine.newTurn()
    framing1.awaitAndSwitchPhase(TurnPhase.Framing)
    val framing2 = engine.newTurn()
    framing2.awaitAndSwitchPhase(TurnPhase.Framing)
    val lock = SerializationGraphTracking.acquireLock(framing1, framing2, UnlockedUnknown)
    framing2.addPredecessor(framing1.selfNode)
    lock.unlock()

    assert(n.incrementFrame(framing2) === FramingBranchEnd) // End because earlier frame by reevaluate turn exists

    n.notify(reevaluate, changed = true)
    n.retrofitSinkFrames(Seq.empty, Some(framing1), -1)
    assert(n.reevOut(reevaluate, Some(Pulse.Value(11))) === FollowFraming(Set.empty, framing2))

    assert(n.incrementSupersedeFrame(framing1, framing2) == Deframe(Set.empty, framing2))
  }
}
