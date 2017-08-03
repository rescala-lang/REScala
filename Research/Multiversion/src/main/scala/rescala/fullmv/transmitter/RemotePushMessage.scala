package rescala.fullmv.transmitter

import rescala.core.{Reactive, WriteableReactive}
import rescala.fullmv.NotificationResultAction.GlitchFreeReady
import rescala.fullmv.tasks.{FullMVAction, ReevaluationResultHandling}
import rescala.fullmv.{FullMVStruct, FullMVTurn, NotificationResultAction}

sealed trait RemotePushMessage[T] {
  val txn: FullMVTurn
  def toTask(localMirrorNode: WriteableReactive[T, FullMVStruct]): FullMVAction
}

case class Framing(txn: FullMVTurn) extends RemotePushMessage[Nothing] {
  override def toTask(localMirrorNode: WriteableReactive[Nothing, FullMVStruct]): FullMVAction = rescala.fullmv.tasks.Framing(txn, localMirrorNode)
}
case class SupersedeFraming(txn: FullMVTurn, supersede: FullMVTurn) extends RemotePushMessage[Nothing] {
  override def toTask(localMirrorNode: WriteableReactive[Nothing, FullMVStruct]): FullMVAction = rescala.fullmv.tasks.SupersedeFraming(txn, localMirrorNode, supersede)
}

case class UnchangedNotification(txn: FullMVTurn) extends RemotePushMessage[Nothing] {
  override def toTask(localMirrorNode: WriteableReactive[Nothing, FullMVStruct]): FullMVAction = rescala.fullmv.tasks.Notification(txn, localMirrorNode, changed = false)
}
case class UnchangedNotificationWithFollowFrame(txn: FullMVTurn, followFrame: FullMVTurn) extends RemotePushMessage[Nothing] {
  override def toTask(localMirrorNode: WriteableReactive[Nothing, FullMVStruct]): FullMVAction = rescala.fullmv.tasks.NotificationWithFollowFrame(txn, localMirrorNode, changed = false, followFrame)
}
case class ChangeNotification[T](txn: FullMVTurn, newValue: T) extends RemotePushMessage[T] {
  override def toTask(localMirrorNode: WriteableReactive[T, FullMVStruct]): FullMVAction = new ChangeNotificationTask(txn, localMirrorNode, newValue) {
    override def deliverNotification(): NotificationResultAction[FullMVTurn, Reactive[FullMVStruct]] = localMirrorNode.state.notify(txn, changed = true)
  }
}
case class ChangeNotificationWithFollowFrame[T](txn: FullMVTurn, newValue: T, followFrame: FullMVTurn) extends RemotePushMessage[T] {
  override def toTask(localMirrorNode: WriteableReactive[T, FullMVStruct]): FullMVAction = new ChangeNotificationTask(txn, localMirrorNode, newValue) {
    override def deliverNotification(): NotificationResultAction[FullMVTurn, Reactive[FullMVStruct]] = localMirrorNode.state.notifyFollowFrame(txn, changed = true, followFrame)
  }
}

abstract class ChangeNotificationTask[T](override val turn: FullMVTurn, override val node: WriteableReactive[T, FullMVStruct], val newValue: T) extends ReevaluationResultHandling {
  override def doCompute(): Unit = {
    val reev = deliverNotification()
    assert(reev == GlitchFreeReady, s"Somehow, $node was not ready for reevaluation after remote change notification by $turn, but returned $reev. Communication channel not FIFO?")
    // Yes, asInstanceOf... don't ask me why the compiler isn't able to prove that these type fit together, when it works
    // just fine in Reevaluation.reevOut(...) (writing a value of type X into WriteableReactive[X, _].state.reevOut(...))
    val res = node.state.reevOut(turn, Some(newValue.asInstanceOf[node.Value]))
    processReevaluationResult(res, changed = true)
  }

  def deliverNotification(): NotificationResultAction[FullMVTurn, Reactive[FullMVStruct]]
}
