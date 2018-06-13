package rescala.fullmv.tasks

import rescala.core.{ReSource, Reactive}
import rescala.fullmv.NotificationResultAction._
import rescala.fullmv._

trait NotificationAction[N <: ReSource[FullMVStruct]] extends ReevaluationHandling[N] {
  val changed: Boolean
  override def doCompute(): Unit = {
    val notificationResultAction = deliverNotification()
    if(FullMVEngine.DEBUG) println(s"[${Thread.currentThread().getName}] $this => $notificationResultAction")
    processNotificationResult(notificationResultAction)
  }

  def processNotificationResult(notificationResultAction: NotificationResultAction[FullMVTurn, Reactive[FullMVStruct]]): Unit = {
    notificationResultAction match {
      case ChangedSomethingInQueue =>
        turn.activeBranchDifferential(TurnPhase.Executing, -1)
      case NotGlitchFreeReady =>
        turn.activeBranchDifferential(TurnPhase.Executing, -1)
      case GlitchFreeReady =>
        doReevaluation()
      case outAndSucc: NotificationOutAndSuccessorOperation[FullMVTurn, Reactive[FullMVStruct]] =>
        processReevOutResult(outAndSucc, changed = false)
    }
  }

  def deliverNotification(): NotificationResultAction[FullMVTurn, Reactive[FullMVStruct]]
}

case class SourceNotification(override val turn: FullMVTurn, override val node: ReSource[FullMVStruct], override val changed: Boolean) extends NotificationAction[ReSource[FullMVStruct]] with SourceReevaluationHandling {
  override def deliverNotification(): NotificationResultAction[FullMVTurn, Reactive[FullMVStruct]] = node.state.notify(turn, changed)
  override def createReevaluation(succTxn: FullMVTurn) = SourceReevaluation(succTxn, node)
}

case class Notification(override val turn: FullMVTurn, override val node: Reactive[FullMVStruct], override val changed: Boolean) extends NotificationAction[Reactive[FullMVStruct]] with RegularReevaluationHandling {
  override def deliverNotification(): NotificationResultAction[FullMVTurn, Reactive[FullMVStruct]] = node.state.notify(turn, changed)
  override def createReevaluation(succTxn: FullMVTurn) = Reevaluation(succTxn, node)
}
case class NotificationWithFollowFrame(override val turn: FullMVTurn, override val node: Reactive[FullMVStruct], override val changed: Boolean, followFrame: FullMVTurn) extends NotificationAction[Reactive[FullMVStruct]] with RegularReevaluationHandling {
  override def deliverNotification(): NotificationResultAction[FullMVTurn, Reactive[FullMVStruct]] = node.state.notifyFollowFrame(turn, changed, followFrame)
  override def createReevaluation(succTxn: FullMVTurn) = Reevaluation(succTxn, node)
}
