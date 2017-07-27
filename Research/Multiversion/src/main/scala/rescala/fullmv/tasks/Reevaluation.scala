package rescala.fullmv.tasks

import rescala.core.{Pulse, Reactive, WriteableReactive}
import rescala.fullmv.NotificationResultAction.NotificationOutAndSuccessorOperation
import rescala.fullmv.NotificationResultAction.NotificationOutAndSuccessorOperation.{FollowFraming, NextReevaluation, NoSuccessor}
import rescala.fullmv._

import scala.util.{Failure, Success, Try}

trait ReevaluationResultHandling extends FullMVAction {
  val node: Reactive[FullMVStruct]
  def processReevaluationResult(outAndSucc: NotificationOutAndSuccessorOperation[FullMVTurn, Reactive[FullMVStruct]], changed: Boolean): Unit = {
    outAndSucc match {
      case NoSuccessor(out) =>
        turn.activeBranchDifferential(TurnPhase.Executing, out.size - 1)
        for (succ <- out) Notification(turn, succ, changed).fork()
      case FollowFraming(out, succTxn) =>
        turn.activeBranchDifferential(TurnPhase.Executing, out.size - 1)
        for (succ <- out) NotificationWithFollowFrame(turn, succ, changed, succTxn).fork()
      case NextReevaluation(out, succTxn) =>
        succTxn.activeBranchDifferential(TurnPhase.Executing, 1)
        turn.activeBranchDifferential(TurnPhase.Executing, out.size - 1)
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
    val result = FullMVEngine.withTurn(turn){ Try { node.reevaluate(turn, node.state.reevIn(turn), node.state.incomings) } }
    result match {
      case Failure(exception) =>
        System.err.println(s"[FullMV Error] Reevaluation of $node failed with ${exception.getClass.getName}: ${exception.getMessage}; Completing reevaluation as NoChange.")
        exception.printStackTrace()
        (node.state.reevOut(turn, None), false)
      case Success(res) =>
        res.commitDependencyDiff()
        if(res.valueChanged) {
          (reevOut(turn, res.commitTuple), true)
        } else {
          (res.node.state.reevOut(turn, None), false)
        }
    }
  }
  @inline
  def reevOut[P](turn: FullMVTurn, commitTuple: (WriteableReactive[Pulse.Change[P], FullMVStruct], Pulse.Change[P])): NotificationOutAndSuccessorOperation[FullMVTurn, Reactive[FullMVStruct]] = {
    val (node, value) = commitTuple
    node.state.reevOut(turn, if (value.isChange) Some(value) else None)
  }
}
