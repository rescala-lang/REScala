package rescala.fullmv.tasks

import rescala.core.Reactive
import rescala.fullmv.NotificationResultAction._
import rescala.fullmv._

trait NotificationAction extends ReevaluationResultHandling {
  val changed: Boolean
  override def doCompute(): Unit = {
    val notificationResultAction = deliverNotification()
    if(FullMVEngine.DEBUG) println(s"[${Thread.currentThread().getName}] $this => $notificationResultAction")
    processNotificationResult(notificationResultAction)
  }

  def processNotificationResult(notificationResultAction: NotificationResultAction[FullMVTurn, Reactive[FullMVStruct]]) = {
    notificationResultAction match {
      case GlitchFreeReadyButQueued =>
        turn.activeBranchDifferential(TurnPhase.Executing, -1)
      case ResolvedNonFirstFrameToUnchanged =>
        turn.activeBranchDifferential(TurnPhase.Executing, -1)
      case NotGlitchFreeReady =>
        turn.activeBranchDifferential(TurnPhase.Executing, -1)
      case GlitchFreeReady =>
        Reevaluation(turn, node).fork
      case outAndSucc: NotificationOutAndSuccessorOperation[FullMVTurn, Reactive[FullMVStruct]] =>
        assert(!changed, s"somehow, $this was digested into an unchanged reevaluation?!")
        processReevaluationResult(outAndSucc, changed)
    }
  }

  def deliverNotification(): NotificationResultAction[FullMVTurn, Reactive[FullMVStruct]]
}

case class Notification(turn: FullMVTurn, node: Reactive[FullMVStruct], changed: Boolean) extends NotificationAction {
  override def deliverNotification(): NotificationResultAction[FullMVTurn, Reactive[FullMVStruct]] = node.state.notify(turn, changed)
}
case class NotificationWithFollowFrame(turn: FullMVTurn, node: Reactive[FullMVStruct], changed: Boolean, followFrame: FullMVTurn) extends NotificationAction {
  override def deliverNotification(): NotificationResultAction[FullMVTurn, Reactive[FullMVStruct]] = node.state.notifyFollowFrame(turn, changed, followFrame)
}
