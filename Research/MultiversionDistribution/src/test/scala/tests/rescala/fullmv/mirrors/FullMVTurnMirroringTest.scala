package tests.rescala.fullmv.mirrors

import org.scalatest.FunSuite
import rescala.fullmv._
import rescala.fullmv.mirrors.localcloning.FullMVTurnLocalClone

import scala.concurrent.Await
import scala.concurrent.duration.Duration

class FullMVTurnMirroringTest extends FunSuite {
  val host0 = new FullMVEngine(Duration.Zero, "0")
  val hostA = new FullMVEngine(Duration.Zero, "A")
  val hostB = new FullMVEngine(Duration.Zero, "B")

  test("instance lookup and equality"){
    val a = host0.newTurn()
    val turn0 = FullMVTurnLocalClone(a, host0)
    assert((turn0 eq a) === true)

    val cloneA = FullMVTurnLocalClone(a, hostA)
    assert((cloneA ne a) === true)
    assert(cloneA === a)
    assert((FullMVTurnLocalClone(a, hostA) eq cloneA) === true)
    assert((FullMVTurnLocalClone(cloneA, hostA) eq cloneA) === true)
    assert((FullMVTurnLocalClone(cloneA, host0) eq a) === true)

    val cloneB = FullMVTurnLocalClone(cloneA, hostB)
    assert((cloneB ne a) === true)
    assert(cloneB === a)
    assert((cloneB ne cloneA) === true)
    assert(cloneB === cloneA)
    assert((FullMVTurnLocalClone(cloneA, hostB) eq cloneB) === true)
    assert((FullMVTurnLocalClone(cloneB, hostB) eq cloneB) === true)
    assert((FullMVTurnLocalClone(cloneB, hostA) eq cloneA) === true)
    assert((FullMVTurnLocalClone(cloneB, host0) eq a) === true)
  }

  test("phase propagation") {
    val turn = host0.newTurn()
    val turnA = FullMVTurnLocalClone(turn, hostA)
    assert(turn.phase === TurnPhase.Uninitialized)
    assert(turnA.phase === TurnPhase.Uninitialized)

    turn.awaitAndSwitchPhase(TurnPhase.Framing)
    assert(turn.phase === TurnPhase.Framing)
    assert(turnA.phase === TurnPhase.Framing)
    val turnB = FullMVTurnLocalClone(turnA, hostB)
    assert(turnB.phase === TurnPhase.Framing)

    turn.awaitAndSwitchPhase(TurnPhase.Executing)
    assert(turn.phase === TurnPhase.Executing)
    assert(turnA.phase === TurnPhase.Executing)
    assert(turnB.phase === TurnPhase.Executing)
  }

  test("branch counting") {
    val turn = host0.newTurn()
    turn.awaitAndSwitchPhase(TurnPhase.Executing)
    turn.activeBranchDifferential(TurnPhase.Executing, 1)

    // turn goes remote to hostA
    val turnA = FullMVTurnLocalClone(turn, hostA)
    turn.addRemoteBranch(TurnPhase.Executing)
    turn.activeBranchDifferential(TurnPhase.Executing, -1)
    turnA.newBranchFromRemote(TurnPhase.Executing)
    assert(turn.activeBranches.get == 1)

    // turn on hostA splits into three branches
    turnA.activeBranchDifferential(TurnPhase.Executing, 1)
    assert(turn.activeBranches.get == 1)

    // first branch goes remote to hostB
    val turnB = FullMVTurnLocalClone(turnA, hostB)
    turnA.addRemoteBranch(TurnPhase.Executing)
    turnA.activeBranchDifferential(TurnPhase.Executing, -1)
    turnB.newBranchFromRemote(TurnPhase.Executing)
    assert(turn.activeBranches.get == 2)

    // second branch goes remote back to host0
    turnA.addRemoteBranch(TurnPhase.Executing)
    turn.newBranchFromRemote(TurnPhase.Executing)
    assert(turn.activeBranches.get == 3)

    // third branch goes remote also to hostB
    turnA.addRemoteBranch(TurnPhase.Executing)
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
    // 0 -> A -> B
    val turnOneRoot = host0.newTurn()
    val turnOne: FullMVTurn = turnOneRoot
    println(s"turnOne on host0: $turnOne with ${turnOne.asInstanceOf[FullMVTurnImpl].subsumableLock.get()}")
    val turnOneA = FullMVTurnLocalClone(turnOne, hostA)
    val turnOneB = FullMVTurnLocalClone(turnOneA, hostB)

    // A -> 0 -> B
    val turnTwoRoot = hostA.newTurn()
    val turnTwoA: FullMVTurn = turnTwoRoot
    println(s"turnTwo on hostA: $turnTwoA with ${turnTwoA.asInstanceOf[FullMVTurnImpl].subsumableLock.get()}")
    val turnTwo = FullMVTurnLocalClone(turnTwoA, host0)
    val turnTwoB = FullMVTurnLocalClone(turnTwo, hostB)

    // B -> A -> 0
    val turnThreeRoot = hostB.newTurn()
    val turnThreeB: FullMVTurn = turnThreeRoot
    println(s"turnThree on hostB: $turnThreeB with ${turnThreeB.asInstanceOf[FullMVTurnImpl].subsumableLock.get()}")
    val turnThreeA = FullMVTurnLocalClone(turnThreeB, hostA)
    val turnThree = FullMVTurnLocalClone(turnThreeA, host0)

    turnOneRoot.awaitAndSwitchPhase(TurnPhase.Executing)
    turnTwoRoot.awaitAndSwitchPhase(TurnPhase.Executing)
    turnThreeRoot.awaitAndSwitchPhase(TurnPhase.Framing)

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

    val locked = SerializationGraphTracking.tryLock(turnOneB, turnTwoB, UnlockedUnknown).asInstanceOf[LockedSameSCC].lock
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

    val locked2 = SerializationGraphTracking.tryLock(turnOneB, turnTwoB, UnlockedUnknown).asInstanceOf[LockedSameSCC].lock
    Await.result(turnThreeA.addPredecessor(turnTwoA.selfNode), Duration.Zero)
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
  }
}
