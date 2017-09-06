package rescala.fullmv.tasks

import rescala.core.Reactive
import rescala.fullmv.NotificationResultAction.NotificationOutAndSuccessorOperation
import rescala.fullmv.NotificationResultAction.NotificationOutAndSuccessorOperation.{FollowFraming, NextReevaluation, NoSuccessor}
import rescala.fullmv._

import scala.util.{Failure, Success, Try}

trait ReevaluationResultHandling extends FullMVAction {
  val node: Reactive[FullMVStruct]
  def processReevaluationResult(outAndSucc: NotificationOutAndSuccessorOperation[FullMVTurn, Reactive[FullMVStruct]], changed: Boolean): Unit = {
    outAndSucc match {
      case NoSuccessor(out) =>
        val branchDiff = out.size - 1
        if(branchDiff != 0) turn.activeBranchDifferential(TurnPhase.Executing, branchDiff)
        for (succ <- out) Notification(turn, succ, changed).fork()
      case FollowFraming(out, succTxn) =>
        val branchDiff = out.size - 1
        if(branchDiff != 0) turn.activeBranchDifferential(TurnPhase.Executing, branchDiff)
        for (succ <- out) NotificationWithFollowFrame(turn, succ, changed, succTxn).fork()
      case NextReevaluation(out, succTxn) =>
        succTxn.activeBranchDifferential(TurnPhase.Executing, 1)
        val branchDiff = out.size - 1
        if(branchDiff != 0) turn.activeBranchDifferential(TurnPhase.Executing, branchDiff)
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
    val result = turn.host.withTurn(turn){ Try { node.reevaluate(turn, node.state.reevIn(turn), node.state.incomings) } }
    result match {
      case Failure(exception) =>
        System.err.println(s"[FullMV Error] Reevaluation of $node failed with ${exception.getClass.getName}: ${exception.getMessage}; Completing reevaluation as NoChange.")
        exception.printStackTrace()
        (node.state.reevOut(turn, None), false)
      case Success(res) =>
        res.commitDependencyDiff(turn, node)
        if(res.valueChanged) {
          (node.state.reevOut(turn, if (res.valueChanged) Some(res.value) else None), true)
        } else {
          (node.state.reevOut(turn, None), false)
        }
    }
  }
}
