package tests.rescala.fullmv

import org.scalatest.FunSuite
import rescala.fullmv._
import rescala.fullmv.mirrors.FullMVTurnLocalClone
import rescala.fullmv.sgt.synchronization.SubsumableLock

class FullMVTurnMirroringTest extends FunSuite {
  case class TurnSet(original: FullMVTurnImpl, mirror1: FullMVTurn, mirror2: FullMVTurn, mirror11: FullMVTurn) {
    val all: Set[FullMVTurn] = Set(original, mirror1, mirror2, mirror11)
    def assertPhase(phase: TurnPhase.Type): Unit = {
      assert(original.phase === phase)
      assert(mirror1.phase === phase)
      assert(mirror2.phase === phase)
      assert(mirror11.phase === phase)
    }

    def assertReachable(turn: FullMVTurn, reachable: Boolean): Unit = {
      assert(original.isTransitivePredecessor(turn) === reachable)
      assert(mirror1.isTransitivePredecessor(turn) === reachable)
      assert(mirror2.isTransitivePredecessor(turn) === reachable)
      assert(mirror11.isTransitivePredecessor(turn) === reachable)
    }

    def assertReachable(turnSet: TurnSet, reachable: Boolean): Unit = {
      assertReachable(turnSet.original, reachable)
      assertReachable(turnSet.mirror1, reachable)
      assertReachable(turnSet.mirror2, reachable)
      assertReachable(turnSet.mirror11, reachable)
    }
  }

  object TurnSet {
    def apply(): TurnSet = {
      val original = new FullMVTurnImpl(null)
      val mirror1 = FullMVTurnLocalClone(original)
      TurnSet(original, mirror1, FullMVTurnLocalClone(original), FullMVTurnLocalClone(mirror1))
    }
  }

  test("equality") {
    val turnSet = TurnSet()
    assert(turnSet.original === turnSet.mirror1)
    assert(turnSet.original === turnSet.mirror2)
    assert(turnSet.original === turnSet.mirror11)

    assert(turnSet.mirror1 === turnSet.original)
    assert(turnSet.mirror1 === turnSet.mirror2)
    assert(turnSet.mirror1 === turnSet.mirror11)

    assert(turnSet.mirror2 === turnSet.mirror1)
    assert(turnSet.mirror2 === turnSet.original)
    assert(turnSet.mirror2 === turnSet.mirror11)

    assert(turnSet.mirror11 === turnSet.mirror1)
    assert(turnSet.mirror11 === turnSet.mirror2)
    assert(turnSet.mirror11 === turnSet.original)
  }

  test("phase propagation") {
    val turnSet = TurnSet()
    turnSet.assertPhase(TurnPhase.Initialized)
    turnSet.original.awaitAndSwitchPhase(TurnPhase.Executing)
    turnSet.assertPhase(TurnPhase.Executing)
    turnSet.original.awaitAndSwitchPhase(TurnPhase.Completed)
    turnSet.assertPhase(TurnPhase.Completed)
  }

  test("predecessor propagation") {
    val turnSetA, turnSetB, turnSetC = TurnSet()
    turnSetA.original.awaitAndSwitchPhase(TurnPhase.Executing)
    turnSetB.original.awaitAndSwitchPhase(TurnPhase.Executing)
    turnSetC.original.awaitAndSwitchPhase(TurnPhase.Framing)

    turnSetA.assertReachable(turnSetB, reachable = false)
    turnSetA.assertReachable(turnSetC, reachable = false)
    turnSetB.assertReachable(turnSetA, reachable = false)
    turnSetB.assertReachable(turnSetC, reachable = false)
    turnSetC.assertReachable(turnSetA, reachable = false)
    turnSetC.assertReachable(turnSetB, reachable = false)

    SubsumableLock.underLock(turnSetA.mirror11, turnSetB.mirror2) {
      assert(DecentralizedSGT.ensureOrder(turnSetA.mirror11, turnSetB.mirror2) === FirstFirst)
    }

    turnSetA.assertReachable(turnSetB, reachable = false)
    turnSetA.assertReachable(turnSetC, reachable = false)
    turnSetB.assertReachable(turnSetA, reachable = true)
    turnSetB.assertReachable(turnSetC, reachable = false)
    turnSetC.assertReachable(turnSetA, reachable = false)
    turnSetC.assertReachable(turnSetB, reachable = false)

    SubsumableLock.underLock(turnSetC.original, turnSetB.mirror1) {
      assert(DecentralizedSGT.ensureOrder(turnSetC.original, turnSetB.mirror1) === SecondFirst)
    }

    turnSetA.assertReachable(turnSetB, reachable = false)
    turnSetA.assertReachable(turnSetC, reachable = false)
    turnSetB.assertReachable(turnSetA, reachable = true)
    turnSetB.assertReachable(turnSetC, reachable = false)
    turnSetC.assertReachable(turnSetA, reachable = true)
    turnSetC.assertReachable(turnSetB, reachable = true)
  }

  test("branch counting") {
    val turnSet = TurnSet()
    turnSet.original.awaitAndSwitchPhase(TurnPhase.Executing)
    turnSet.original.activeBranchDifferential(TurnPhase.Executing, 1)
    turnSet.mirror2.activeBranchDifferential(TurnPhase.Executing, 1)
    assert(turnSet.original.activeBranches.get == 1)

    turnSet.mirror2.activeBranchDifferential(TurnPhase.Executing, -1)
    assert(turnSet.original.activeBranches.get == 0)

    turnSet.original.activeBranchDifferential(TurnPhase.Executing, 2)
    turnSet.mirror1.activeBranchDifferential(TurnPhase.Executing, 1)
    turnSet.mirror11.activeBranchDifferential(TurnPhase.Executing, 1)
    turnSet.mirror2.activeBranchDifferential(TurnPhase.Executing, 1)
    assert(turnSet.original.activeBranches.get == 2)

    turnSet.mirror2.activeBranchDifferential(TurnPhase.Executing, -1)
    assert(turnSet.original.activeBranches.get == 1)

    turnSet.mirror11.activeBranchDifferential(TurnPhase.Executing, -1)
    assert(turnSet.original.activeBranches.get == 0)
  }
}
