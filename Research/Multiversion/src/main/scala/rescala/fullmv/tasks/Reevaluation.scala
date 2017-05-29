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
  override def doCompute(): Unit = {
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
        diff.added.foreach { discover =>
          val (successorWrittenVersions, maybeFollowFrame) = discover.state.discover(turn, node)
          if(FullMVEngine.DEBUG) println(s"[${Thread.currentThread().getName}] $this discovering $discover-> $node re-queueing $successorWrittenVersions and re-framing $maybeFollowFrame")
          node.state.retrofitSinkFrames(successorWrittenVersions, maybeFollowFrame, 1)
        }
        node.state.incomings = deps
        (node.state.reevOut(turn, if (isChange) Some(value) else None), isChange)
    }
  }
}
