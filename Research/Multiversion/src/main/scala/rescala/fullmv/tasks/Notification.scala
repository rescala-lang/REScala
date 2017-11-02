package rescala.fullmv.tasks

import rescala.core.Reactive
import rescala.fullmv.NotificationResultAction._
import rescala.fullmv._

trait NotificationAction extends FullMVAction {
  val node: Reactive[FullMVStruct]
  val changed: Boolean
  override def doCompute(): Traversable[FullMVAction] = {
    val notificationResultAction = deliverNotification()
    if(FullMVEngine.DEBUG) println(s"[${Thread.currentThread().getName}] $this => $notificationResultAction")
    processNotificationResult(notificationResultAction)
  }

  def processNotificationResult(notificationResultAction: NotificationResultAction[FullMVTurn, Reactive[FullMVStruct]]): Traversable[FullMVAction] = {
    notificationResultAction match {
      case GlitchFreeReadyButQueued =>
        turn.activeBranchDifferential(TurnPhase.Executing, -1)
        Traversable.empty
      case ResolvedNonFirstFrameToUnchanged =>
        turn.activeBranchDifferential(TurnPhase.Executing, -1)
        Traversable.empty
      case NotGlitchFreeReady =>
        turn.activeBranchDifferential(TurnPhase.Executing, -1)
        Traversable.empty
      case GlitchFreeReady =>
        Traversable(Reevaluation(turn, node))
      case outAndSucc: NotificationOutAndSuccessorOperation[FullMVTurn, Reactive[FullMVStruct]] =>
        Reevaluation.processReevaluationResult(node, turn, outAndSucc, changed = false)
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
