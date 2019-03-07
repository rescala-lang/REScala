package rescala.fullmv.tasks

import rescala.core.{ReSource, Derived}
import rescala.fullmv.NotificationResultAction._
import rescala.fullmv._

trait NotificationAction[N <: ReSource[FullMVStruct]] extends ReevaluationHandling[N] {
  val changed: Boolean
  override def doCompute(): Unit = {
    val (retainBranch, notificationResultAction) = deliverNotification()
    if(FullMVEngine.DEBUG) println(s"[${Thread.currentThread().getName}] $this => $retainBranch, $notificationResultAction")
    processNotificationResult(retainBranch, notificationResultAction)
  }

  def processNotificationResult(retainBranch: Boolean, notificationResultAction: NotificationResultAction[FullMVTurn, Derived[FullMVStruct]]): Unit = {
    notificationResultAction match {
      case DoNothing                                                                           =>
        if(!retainBranch) turn.activeBranchDifferential(TurnPhase.Executing, -1)
      case ReevaluationReady                                                                   =>
        doReevaluation(retainBranch)
      case outAndSucc: NotificationOutAndSuccessorOperation[FullMVTurn, Derived[FullMVStruct]] =>
        processReevOutResult(retainBranch, outAndSucc, changed = false)
    }
  }

  def deliverNotification(): (Boolean, NotificationResultAction[FullMVTurn, Derived[FullMVStruct]])
}

class SourceNotification(override val turn: FullMVTurn, override val node: ReSource[FullMVStruct], override val changed: Boolean) extends NotificationAction[ReSource[FullMVStruct]] with SourceReevaluationHandling {
  override def deliverNotification(): (Boolean, NotificationResultAction[FullMVTurn, Derived[FullMVStruct]]) = node.state.notify(turn, changed)
  override def createReevaluation(succTxn: FullMVTurn) = new SourceReevaluation(succTxn, node)
  override def toString = s"SourceNotification($turn, $node)"
}

class Notification(override val turn: FullMVTurn, override val node: Derived[FullMVStruct], override val changed: Boolean) extends NotificationAction[Derived[FullMVStruct]] with RegularReevaluationHandling {
  override def deliverNotification(): (Boolean, NotificationResultAction[FullMVTurn, Derived[FullMVStruct]]) = node.state.notify(turn, changed)
  override def createReevaluation(succTxn: FullMVTurn) = new Reevaluation(succTxn, node)
  override def toString = s"Notification($turn, $node)"
}
class NotificationWithFollowFrame(override val turn: FullMVTurn, override val node: Derived[FullMVStruct], override val changed: Boolean, followFrame: FullMVTurn) extends NotificationAction[Derived[FullMVStruct]] with RegularReevaluationHandling {
  override def deliverNotification(): (Boolean, NotificationResultAction[FullMVTurn, Derived[FullMVStruct]]) = node.state.notifyFollowFrame(turn, changed, followFrame)
  override def createReevaluation(succTxn: FullMVTurn) = new Reevaluation(succTxn, node)
  override def toString = s"NotificationWithFollowFrame($turn, $node, $followFrame)"
}
