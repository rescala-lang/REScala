package rescala.fullmv.transmitter

import rescala.fullmv.tasks.FullMVAction
import rescala.fullmv.FullMVTurn

sealed trait RemotePushMessage[+T] {
  val txn: FullMVTurn
  def toTask(localMirrorNode: Reflection[T]): FullMVAction
}

case class Framing(txn: FullMVTurn) extends RemotePushMessage[Nothing] {
  override def toTask(localMirrorNode: Reflection[Nothing]): FullMVAction = rescala.fullmv.tasks.Framing(txn, localMirrorNode)
}
case class SupersedeFraming(txn: FullMVTurn, supersede: FullMVTurn) extends RemotePushMessage[Nothing] {
  override def toTask(localMirrorNode: Reflection[Nothing]): FullMVAction = rescala.fullmv.tasks.SupersedeFraming(txn, localMirrorNode, supersede)
}

sealed trait NotificationMessage[+T] extends RemotePushMessage[T] {
  val changed: Boolean
}
sealed trait NoFollowFrameNotificationMessage[+T] extends NotificationMessage[T] {
  override def toTask(localMirrorNode: Reflection[T]): FullMVAction = rescala.fullmv.tasks.Notification(txn, localMirrorNode, changed)
}
sealed trait NotificationWithFollowFrameMessage[+T] extends NotificationMessage[T] {
  val followFrame: FullMVTurn
  override def toTask(localMirrorNode: Reflection[T]): FullMVAction = rescala.fullmv.tasks.NotificationWithFollowFrame(txn, localMirrorNode, changed, followFrame)
}

sealed trait UnChangedMessage extends NotificationMessage[Nothing] {
  override val changed = false
}
sealed trait ChangeMessage[+T] extends NotificationMessage[T] {
  override val changed = true
  val newValue: T
  abstract override def toTask(localMirrorNode: Reflection[T]): FullMVAction = {
    localMirrorNode.buffer(txn, newValue)
    super.toTask(localMirrorNode)
  }
}

case class UnchangedNotification(txn: FullMVTurn) extends NoFollowFrameNotificationMessage[Nothing] with UnChangedMessage
case class UnchangedNotificationWithFollowFrame(txn: FullMVTurn, followFrame: FullMVTurn) extends NotificationWithFollowFrameMessage[Nothing] with UnChangedMessage

case class ChangeNotification[T](txn: FullMVTurn, newValue: T) extends NoFollowFrameNotificationMessage[T] with ChangeMessage[T]
case class ChangeNotificationWithFollowFrame[T](txn: FullMVTurn, newValue: T, followFrame: FullMVTurn) extends NotificationWithFollowFrameMessage[T] with ChangeMessage[T]
