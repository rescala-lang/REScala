package rescala.fullmv.tasks

import rescala.fullmv.NotificationResultAction.NotificationOutAndSuccessorOperation
import rescala.fullmv.NotificationResultAction.NotificationOutAndSuccessorOperation.{FollowFraming, NextReevaluation, NoSuccessor}
import rescala.fullmv._
import rescala.graph.Reactive
import rescala.graph.ReevaluationResult.{Dynamic, Static}

import scala.util.{Failure, Success, Try}

trait ReevaluationResultHandling extends FullMVAction {
  val node: Reactive[FullMVStruct]
  def processReevaluationResult(outAndSucc: NotificationOutAndSuccessorOperation[FullMVTurn, Reactive[FullMVStruct]], changed: Boolean) = {
    if(changed) synchronized { turn.completedReevaluations.incrementAndGet() }
    outAndSucc match {
      case NoSuccessor(out) =>
        if (out.size != 1) turn.activeBranchDifferential(TurnPhase.Executing, out.size - 1)
        for (succ <- out) Notification(turn, succ, changed).fork()
      case FollowFraming(out, succTxn) =>
        if (out.size != 1) turn.activeBranchDifferential(TurnPhase.Executing, out.size - 1)
        for (succ <- out) NotificationWithFollowFrame(turn, succ, changed, succTxn).fork()
      case NextReevaluation(out, succTxn) =>
        succTxn.activeBranchDifferential(TurnPhase.Executing, 1)
        if (out.size != 1) turn.activeBranchDifferential(TurnPhase.Executing, out.size - 1)
        for (succ <- out) NotificationWithFollowFrame(turn, succ, changed, succTxn).fork()
        Reevaluation(succTxn, node).fork()
    }
  }
}

case class Reevaluation(turn: FullMVTurn, node: Reactive[FullMVStruct]) extends ReevaluationResultHandling {
  override def compute(): Unit = {
    val (outAndSucc, changed) = Reevaluation.doReevaluation(turn, node)
    if(FullMVEngine.DEBUG) println(s"[${Thread.currentThread().getName}] $this => ${if(changed) "changed" else "unchanged"} $outAndSucc")
    processReevaluationResult(outAndSucc, changed)
  }
}

object Reevaluation {
  def doReevaluation(turn: FullMVTurn, node: Reactive[FullMVStruct]): (NotificationOutAndSuccessorOperation[FullMVTurn, Reactive[FullMVStruct]], Boolean) = {
    assert(turn.phase == TurnPhase.Executing, s"$this cannot reevaluate (requires executing phase")
    val result = FullMVEngine.withTurn(turn){ Try { node.reevaluate(turn) } }
    result match {
      case Failure(exception) =>
        System.err.println(s"[FullMV Error] Reevaluation of $node failed with ${exception.getClass.getName}: ${exception.getMessage}; Completing reevaluation as NoChange.")
        exception.printStackTrace()
        (node.state.reevOut(turn, None), false)
      case Success(Static(isChange, value)) =>
        (node.state.reevOut(turn, if(isChange) Some(value) else None), isChange)
      case Success(res @ Dynamic(isChange, value, deps)) =>
        val diff = res.depDiff(node.state.incomings)
        diff.removed.foreach{ drop =>
          val (successorWrittenVersions, maybeFollowFrame) = drop.state.drop(turn, node)
          if(FullMVEngine.DEBUG) println(s"[${Thread.currentThread().getName}] $this dropping $drop -> $node un-queueing $successorWrittenVersions and un-framing $maybeFollowFrame")
          node.state.retrofitSinkFrames(successorWrittenVersions, maybeFollowFrame, -1)
        }
        // TODO after adding in-turn parallelism, nodes may be more complete here than they were during actual reevaluation, leading to missed glitches
        val anyPendingDependency = diff.added.foldLeft(false) { (anyPendingDependency, discover) =>
          val (successorWrittenVersions, maybeFollowFrame) = discover.state.discover(turn, node)
          if(FullMVEngine.DEBUG) println(s"[${Thread.currentThread().getName}] $this discovering $discover-> $node re-queueing $successorWrittenVersions and re-framing $maybeFollowFrame")
          node.state.retrofitSinkFrames(successorWrittenVersions, maybeFollowFrame, 1)
          // this does not check for a frame by any preceding transaction but only for itself, because
          // the synchronization at the beginning of discover makes it so that no preceding frames exist;
          // should that be removed, we need to instead check, e.g.,
          // maybeFollowFrame.exists{txn => txn == this || sgt.getOrder(txn, this) == FirstFirst}
          anyPendingDependency || (maybeFollowFrame == Some(this))
        }
        // if(droppedOwnFrame) {
        //   if(rediscoveredOwnFrame) {
        //     System.err.println(s"[FullMV Warning] reevaluation of $node during $this re-routed all its incoming changed edges. Not sure if this should be legal.")
        //   } else {
        //     assert(!isChange, s"Impossible Reevaluation of $node during $this: Dropped all incoming changed edges, but still produced a change!")
        //     System.err.println(s"[FullMV Warning] reevaluation (unchanged) of $node during $this dropped all its incoming changed edges. This should probably be illegal, but dynamic events are implemented badly, causing this.")
        //   }
        // } else {
        //   assert(!rediscoveredOwnFrame, "either this is impossible or I am stupid.")
        // }
        node.state.incomings = deps
        if(anyPendingDependency) {
          assert(false, "this should no longer be happening.")
          (NoSuccessor(Set.empty), false)
        } else {
          (node.state.reevOut(turn, if (isChange) Some(value) else None), isChange)
        }
    }
  }
}
