package reactives.fullmv

sealed trait FramingBranchResult[+T, +R]
object FramingBranchResult {
  case object FramingBranchEnd                                         extends FramingBranchResult[Nothing, Nothing]
  case class Frame[T, R](out: Set[R], frame: T)                        extends FramingBranchResult[T, R]
  case class FrameSupersede[T, R](out: Set[R], frame: T, supersede: T) extends FramingBranchResult[T, R]
}

sealed trait NotificationBranchResult[+T, +R]
object NotificationBranchResult {
  case object DoNothing         extends NotificationBranchResult[Nothing, Nothing]
  case object ReevaluationReady extends NotificationBranchResult[Nothing, Nothing]
  sealed trait ReevOutBranchResult[+T, R] extends NotificationBranchResult[T, R] {
    val out: Set[R]
  }
  object ReevOutBranchResult {
    case class PureNotifyOnly[R](out: Set[R])                            extends ReevOutBranchResult[Nothing, R]
    case class NotifyAndNonReadySuccessor[T, R](out: Set[R], succTxn: T) extends ReevOutBranchResult[T, R]
    case class NotifyAndReevaluationReadySuccessor[T, R](out: Set[R], succTxn: T) extends ReevOutBranchResult[T, R]
  }
}
