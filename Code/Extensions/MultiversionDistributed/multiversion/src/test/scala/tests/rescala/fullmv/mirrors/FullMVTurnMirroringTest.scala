package tests.rescala.fullmv.mirrors

import org.scalatest.funsuite.AnyFunSuite
import rescala.fullmv._
import DistributedFullMVApi.{FullMVTurnLocalClone, FullMVEngine, FullMVTurn, FullMVTurnImpl, SerializationGraphTracking}

import scala.concurrent.Await
import scala.concurrent.duration.Duration

class FullMVTurnMirroringTest extends AnyFunSuite {

  test("instance lookup") {
    val host0 = new FullMVEngine(Duration.Zero, "0")
    val hostA = new FullMVEngine(Duration.Zero, "A")
    val hostB = new FullMVEngine(Duration.Zero, "B")

    val a = host0.newTurn()
    a.beginExecuting()
    val turn0 = FullMVTurnLocalClone(a, host0)
    assert(turn0 === a)

    val cloneA = FullMVTurnLocalClone(a, hostA)
    assert(cloneA !== a)
    assert(cloneA.remotelyEquals(a))
    assert(FullMVTurnLocalClone(a, hostA) === cloneA)
    assert(FullMVTurnLocalClone(cloneA, hostA) === cloneA)
    assert(FullMVTurnLocalClone(cloneA, host0) === a)

    val cloneB = FullMVTurnLocalClone(cloneA, hostB)
    assert(cloneB !== a)
    assert(cloneB.remotelyEquals(a))
    assert(cloneB !== cloneA)
    assert(cloneB.remotelyEquals(cloneA))
    assert(FullMVTurnLocalClone(cloneA, hostB) === cloneB)
    assert(FullMVTurnLocalClone(cloneB, hostB) === cloneB)
    assert(FullMVTurnLocalClone(cloneB, hostA) === cloneA)
    assert(FullMVTurnLocalClone(cloneB, host0) === a)
  }

  test("phase propagation") {
    val host0 = new FullMVEngine(Duration.Zero, "0")
    val hostA = new FullMVEngine(Duration.Zero, "A")
    val hostB = new FullMVEngine(Duration.Zero, "B")

    val turn = host0.newTurn()
    turn.beginFraming()

    val turnA = FullMVTurnLocalClone(turn, hostA)
    assert(turn.phase === TurnPhase.Framing)
    assert(turnA.phase === TurnPhase.Framing)
    val turnB = FullMVTurnLocalClone(turnA, hostB)
    assert(turnB.phase === TurnPhase.Framing)

    turn.completeFraming()
    assert(turn.phase === TurnPhase.Executing)
    assert(turnA.phase === TurnPhase.Executing)
    assert(turnB.phase === TurnPhase.Executing)
  }

  test("branch counting") {
    val host0 = new FullMVEngine(Duration.Zero, "0")
    val hostA = new FullMVEngine(Duration.Zero, "A")
    val hostB = new FullMVEngine(Duration.Zero, "B")

    val turn = host0.newTurn()
    turn.beginExecuting()
    turn.activeBranchDifferential(TurnPhase.Executing, 1)

    // turn goes remote to hostA
    val turnA = FullMVTurnLocalClone(turn, hostA)
    Await.result(turn.addRemoteBranch(TurnPhase.Executing), Duration.Zero)
    turn.activeBranchDifferential(TurnPhase.Executing, -1)
    turnA.newBranchFromRemote(TurnPhase.Executing)
    assert(turn.activeBranches.get == 1)

    // turn on hostA splits into three branches
    turnA.activeBranchDifferential(TurnPhase.Executing, 1)
    assert(turn.activeBranches.get == 1)

    // first branch goes remote to hostB
    val turnB = FullMVTurnLocalClone(turnA, hostB)
    Await.result(turnA.addRemoteBranch(TurnPhase.Executing), Duration.Zero)
    turnA.activeBranchDifferential(TurnPhase.Executing, -1)
    turnB.newBranchFromRemote(TurnPhase.Executing)
    assert(turn.activeBranches.get == 2)

    // second branch goes remote back to host0
    Await.result(turnA.addRemoteBranch(TurnPhase.Executing), Duration.Zero)
    turn.newBranchFromRemote(TurnPhase.Executing)
    assert(turn.activeBranches.get == 3)

    // third branch goes remote also to hostB
    Await.result(turnA.addRemoteBranch(TurnPhase.Executing), Duration.Zero)
    turnA.activeBranchDifferential(TurnPhase.Executing, -1)
    turnB.newBranchFromRemote(TurnPhase.Executing)
    assert(turn.activeBranches.get == 2)

    // branch on host0 terminates
    turn.activeBranchDifferential(TurnPhase.Executing, -1)
    assert(turn.activeBranches.get == 1)

    // One branch on hostB terminates
    turnB.activeBranchDifferential(TurnPhase.Executing, -1)
    assert(turn.activeBranches.get == 1)

    // Other branch on hostB terminates
    turnB.activeBranchDifferential(TurnPhase.Executing, -1)
    assert(turn.activeBranches.get == 0)
  }

  test("predecessor propagation") {
    val host0 = new FullMVEngine(Duration.Zero, "0")
    val hostA = new FullMVEngine(Duration.Zero, "A")
    val hostB = new FullMVEngine(Duration.Zero, "B")

    // 0 -> A -> B
    val turnOneRoot = host0.newTurn()
    turnOneRoot.beginExecuting()
    val turnOne: FullMVTurn = turnOneRoot
    if (FullMVUtil.DEBUG)
      println(s"turnOne on host0: $turnOne with ${turnOne.asInstanceOf[FullMVTurnImpl].subsumableLock.get()}")
    val turnOneA = FullMVTurnLocalClone.withPredecessorReplication(turnOne, hostA)
    val turnOneB = FullMVTurnLocalClone.withPredecessorReplication(turnOneA, hostB)

    // A -> 0 -> B
    val turnTwoRoot = hostA.newTurn()
    turnTwoRoot.beginExecuting()
    val turnTwoA: FullMVTurn = turnTwoRoot
    if (FullMVUtil.DEBUG)
      println(s"turnTwo on hostA: $turnTwoA with ${turnTwoA.asInstanceOf[FullMVTurnImpl].subsumableLock.get()}")
    val turnTwo  = FullMVTurnLocalClone.withPredecessorReplication(turnTwoA, host0)
    val turnTwoB = FullMVTurnLocalClone.withPredecessorReplication(turnTwo, hostB)

    // B -> A -> 0
    val turnThreeRoot = hostB.newTurn()
    turnThreeRoot.beginExecuting()
    val turnThreeB: FullMVTurn = turnThreeRoot
    if (FullMVUtil.DEBUG)
      println(s"turnThree on hostB: $turnThreeB with ${turnThreeB.asInstanceOf[FullMVTurnImpl].subsumableLock.get()}")
    val turnThreeA = FullMVTurnLocalClone.withPredecessorReplication(turnThreeB, hostA)
    val turnThree  = FullMVTurnLocalClone.withPredecessorReplication(turnThreeA, host0)

    assert(turnOne.isTransitivePredecessor(turnTwo) === false)
    assert(turnOne.isTransitivePredecessor(turnThree) === false)
    assert(turnTwo.isTransitivePredecessor(turnOne) === false)
    assert(turnTwo.isTransitivePredecessor(turnThree) === false)
    assert(turnThree.isTransitivePredecessor(turnOne) === false)
    assert(turnThree.isTransitivePredecessor(turnTwo) === false)

    assert(turnOneA.isTransitivePredecessor(turnTwoA) === false)
    assert(turnOneA.isTransitivePredecessor(turnThreeA) === false)
    assert(turnTwoA.isTransitivePredecessor(turnOneA) === false)
    assert(turnTwoA.isTransitivePredecessor(turnThreeA) === false)
    assert(turnThreeA.isTransitivePredecessor(turnOneA) === false)
    assert(turnThreeA.isTransitivePredecessor(turnTwoA) === false)

    assert(turnOneB.isTransitivePredecessor(turnTwoB) === false)
    assert(turnOneB.isTransitivePredecessor(turnThreeB) === false)
    assert(turnTwoB.isTransitivePredecessor(turnOneB) === false)
    assert(turnTwoB.isTransitivePredecessor(turnThreeB) === false)
    assert(turnThreeB.isTransitivePredecessor(turnOneB) === false)
    assert(turnThreeB.isTransitivePredecessor(turnTwoB) === false)

    val locked =
      SerializationGraphTracking.tryLock(turnOneB, turnTwoB, UnlockedUnknown).asInstanceOf[LockedSameSCC].lock
    Await.result(turnTwoB.addPredecessor(turnOneB.selfNode), Duration.Zero)
    locked.asyncUnlock()

    assert(turnOne.isTransitivePredecessor(turnTwo) === false)
    assert(turnOne.isTransitivePredecessor(turnThree) === false)
    assert(turnTwo.isTransitivePredecessor(turnOne) === true)
    assert(turnTwo.isTransitivePredecessor(turnThree) === false)
    assert(turnThree.isTransitivePredecessor(turnOne) === false)
    assert(turnThree.isTransitivePredecessor(turnTwo) === false)

    assert(turnOneA.isTransitivePredecessor(turnTwoA) === false)
    assert(turnOneA.isTransitivePredecessor(turnThreeA) === false)
    assert(turnTwoA.isTransitivePredecessor(turnOneA) === true)
    assert(turnTwoA.isTransitivePredecessor(turnThreeA) === false)
    assert(turnThreeA.isTransitivePredecessor(turnOneA) === false)
    assert(turnThreeA.isTransitivePredecessor(turnTwoA) === false)

    assert(turnOneB.isTransitivePredecessor(turnTwoB) === false)
    assert(turnOneB.isTransitivePredecessor(turnThreeB) === false)
    assert(turnTwoB.isTransitivePredecessor(turnOneB) === true)
    assert(turnTwoB.isTransitivePredecessor(turnThreeB) === false)
    assert(turnThreeB.isTransitivePredecessor(turnOneB) === false)
    assert(turnThreeB.isTransitivePredecessor(turnTwoB) === false)

    val hostC      = new FullMVEngine(Duration.Zero, "C")
    val turnOneC   = FullMVTurnLocalClone.withPredecessorReplication(turnOneA, hostC)
    val turnTwoC   = FullMVTurnLocalClone.withPredecessorReplication(turnTwoA, hostC)
    val turnThreeC = FullMVTurnLocalClone.withPredecessorReplication(turnThreeA, hostC)

    assert(turnOneC.isTransitivePredecessor(turnTwoC) === false)
    assert(turnOneC.isTransitivePredecessor(turnThreeC) === false)
    assert(turnTwoC.isTransitivePredecessor(turnOneC) === true)
    assert(turnTwoC.isTransitivePredecessor(turnThreeC) === false)
    assert(turnThreeC.isTransitivePredecessor(turnOneC) === false)
    assert(turnThreeC.isTransitivePredecessor(turnTwoC) === false)

    val locked2 =
      SerializationGraphTracking.tryLock(turnThreeC, turnTwoC, UnlockedUnknown).asInstanceOf[LockedSameSCC].lock
    Await.result(turnThreeC.addPredecessor(turnTwoC.selfNode), Duration.Zero)
    locked2.asyncUnlock()

    assert(turnOne.isTransitivePredecessor(turnTwo) === false)
    assert(turnOne.isTransitivePredecessor(turnThree) === false)
    assert(turnTwo.isTransitivePredecessor(turnOne) === true)
    assert(turnTwo.isTransitivePredecessor(turnThree) === false)
    assert(turnThree.isTransitivePredecessor(turnOne) === true)
    assert(turnThree.isTransitivePredecessor(turnTwo) === true)

    assert(turnOneA.isTransitivePredecessor(turnTwoA) === false)
    assert(turnOneA.isTransitivePredecessor(turnThreeA) === false)
    assert(turnTwoA.isTransitivePredecessor(turnOneA) === true)
    assert(turnTwoA.isTransitivePredecessor(turnThreeA) === false)
    assert(turnThreeA.isTransitivePredecessor(turnOneA) === true)
    assert(turnThreeA.isTransitivePredecessor(turnTwoA) === true)

    assert(turnOneB.isTransitivePredecessor(turnTwoB) === false)
    assert(turnOneB.isTransitivePredecessor(turnThreeB) === false)
    assert(turnTwoB.isTransitivePredecessor(turnOneB) === true)
    assert(turnTwoB.isTransitivePredecessor(turnThreeB) === false)
    assert(turnThreeB.isTransitivePredecessor(turnOneB) === true)
    assert(turnThreeB.isTransitivePredecessor(turnTwoB) === true)

    assert(turnOneC.isTransitivePredecessor(turnTwoC) === false)
    assert(turnOneC.isTransitivePredecessor(turnThreeC) === false)
    assert(turnTwoC.isTransitivePredecessor(turnOneC) === true)
    assert(turnTwoC.isTransitivePredecessor(turnThreeC) === false)
    assert(turnThreeC.isTransitivePredecessor(turnOneC) === true)
    assert(turnThreeC.isTransitivePredecessor(turnTwoC) === true)
  }
}
