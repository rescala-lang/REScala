package rescala.fullmv.transmitter

import rescala.fullmv.sgt.synchronization.SubsumableLock
import rescala.fullmv.sgt.synchronization.SubsumableLock.TryLockResult
import retier.transmitter._

class SubsumableLockTransmittable {
  import SubsumableLockTransmittable._
  implicit def subsumableLockTransmittable[S](implicit messageTransmittable: Transmittable[Message, S, Message], serializable: Serializable[S]): Transmittable[SubsumableLock, S, SubsumableLock] = ???
}

object SubsumableLockTransmittable extends SubsumableLockTransmittable {
  type Message = RemoteObjectTransmittable.Type[Unit, Nothing, (Int, Boolean, SubsumableLock, Long), (Boolean, SubsumableLock, Long)]
  private val toTryLockResult: ((Boolean, SubsumableLock, Long)) => TryLockResult = (TryLockResult.apply _).tupled
  private val fromTryLockResult: TryLockResult => (Boolean, SubsumableLock, Long) = TryLockResult.unapply(_).get
  private val noReturnValue = (false, null, 0L)
  //
//  sealed trait OperationResponse
//  case class LockResponse(res: TryLockResult) extends OperationResponse
//  case class TryLockResponse(res: TryLockResult) extends OperationResponse
//  case class GetLockedRootResponse(res: Option[SubsumableLock.GUID]) extends OperationResponse
//  case class SubsumeResponse(res: TryLockResult) extends OperationResponse
//  case object UnlockResponse extends OperationResponse
//
//  sealed trait OperationRequest {
//    def dispatch(localInstance: SubsumableLock): OperationResponse
//  }
//  case object LockRequest extends OperationRequest {
//    override def dispatch(localInstance: SubsumableLock): OperationResponse = LockResponse(localInstance.lock())
//  }
//  case object TryLockRequest extends OperationRequest {
//    override def dispatch(localInstance: SubsumableLock): OperationResponse = TryLockResponse(localInstance.tryLock())
//  }
//  case object GetLockedRootRequest extends OperationRequest {
//    override def dispatch(localInstance: SubsumableLock): OperationResponse = GetLockedRootResponse(localInstance.getLockedRoot)
//  }
//  case class SubsumeRequest(lock: TryLockResult) extends OperationRequest {
//    override def dispatch(localInstance: SubsumableLock): OperationResponse = SubsumeResponse(localInstance.subsume(lock))
//  }
//  case object UnlockRequest extends OperationRequest {
//    override def dispatch(localInstance: SubsumableLock): OperationResponse = { localInstance.unlock(); UnlockResponse }
//  }
}
