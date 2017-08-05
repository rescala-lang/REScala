package rescala.fullmv.transmitter

import rescala.fullmv.sgt.synchronization.SubsumableLock
import rescala.fullmv.sgt.synchronization.SubsumableLock.TryLockResult
import rescala.fullmv.transmitter.SubsumableLockTransmittable.{OperationRequest, OperationResponse}
import retier.transmitter._

class SubsumableLockTransmittable {
  type M = RemoteObjectMessaging[Unit, Nothing, OperationRequest, OperationResponse]
  import SubsumableLockTransmittable._
  implicit def subsumableLockTransmittable[S](implicit messageTransmittable: Transmittable[M, S, M], serializable: Serializable[S]): Transmittable[SubsumableLock, S, SubsumableLock] = {
    new RemoteObjectTransmittable[SubsumableLock, SubsumableLock, Unit, Nothing, OperationRequest, OperationResponse, S] {
      override def dispatch(localInstance: SubsumableLock, request: OperationRequest): OperationResponse = { request.dispatch(localInstance) }
      override def createInitializationAndConnectUpdates(value: SubsumableLock, update: (Nothing) => Unit): Unit = Unit
      override def instantiateMirror(value: Unit, request: (OperationRequest) => OperationResponse): SubsumableLock = new SubsumableLock {
        override def unlock(): Unit = request(UnlockRequest)
        override def tryLock(): TryLockResult = request(TryLockRequest).asInstanceOf[TryLockResponse].res
        override def subsume(subsumableLock: TryLockResult): TryLockResult = request(SubsumeRequest(subsumableLock)).asInstanceOf[SubsumeResponse].res
        override def lock(): TryLockResult = request(LockRequest).asInstanceOf[LockResponse].res
        override def getLockedRoot: Option[SubsumableLock.GUID] = request(GetLockedRootRequest).asInstanceOf[GetLockedRootResponse].res
      }
      override def updateMirror(mirror: SubsumableLock, update: Nothing): Unit = throw new NotImplementedError()
    }
  }
}

object SubsumableLockTransmittable extends SubsumableLockTransmittable {
  sealed trait OperationResponse
  case class LockResponse(res: TryLockResult) extends OperationResponse
  case class TryLockResponse(res: TryLockResult) extends OperationResponse
  case class GetLockedRootResponse(res: Option[SubsumableLock.GUID]) extends OperationResponse
  case class SubsumeResponse(res: TryLockResult) extends OperationResponse
  case object UnlockResponse extends OperationResponse

  sealed trait OperationRequest {
    def dispatch(localInstance: SubsumableLock): OperationResponse
  }
  case object LockRequest extends OperationRequest {
    override def dispatch(localInstance: SubsumableLock): OperationResponse = LockResponse(localInstance.lock())
  }
  case object TryLockRequest extends OperationRequest {
    override def dispatch(localInstance: SubsumableLock): OperationResponse = TryLockResponse(localInstance.tryLock())
  }
  case object GetLockedRootRequest extends OperationRequest {
    override def dispatch(localInstance: SubsumableLock): OperationResponse = GetLockedRootResponse(localInstance.getLockedRoot)
  }
  case class SubsumeRequest(lock: TryLockResult) extends OperationRequest {
    override def dispatch(localInstance: SubsumableLock): OperationResponse = SubsumeResponse(localInstance.subsume(lock))
  }
  case object UnlockRequest extends OperationRequest {
    override def dispatch(localInstance: SubsumableLock): OperationResponse = { localInstance.unlock(); UnlockResponse }
  }
}
