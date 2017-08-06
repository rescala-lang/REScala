package rescala.fullmv.transmitter

import rescala.fullmv.sgt.synchronization.SubsumableLock
import rescala.fullmv.sgt.synchronization.SubsumableLock.TryLockResult
import retier.transmitter._

class SubsumableLockTransmittable {
  import SubsumableLockTransmittable._
  implicit def subsumableLockTransmittable[S](implicit messageTransmittable: Transmittable[Message, S, Message], serializable: Serializable[S]): Transmittable[SubsumableLock, S, SubsumableLock] = {
    new RemoteObjectTransmittable[SubsumableLock, SubsumableLock, Unit, Nothing, (Int, Boolean, SubsumableLock, Long), (Boolean, SubsumableLock, Long), S] {
      override def dispatch(localInstance: SubsumableLock, request: (Int, Boolean, SubsumableLock, Long)): (Boolean, SubsumableLock, Long) = { request._1 match {
        case 0 => fromTryLockResult(localInstance.lock())
        case 1 => fromTryLockResult(localInstance.tryLock())
        case 2 =>
          val res = localInstance.getLockedRoot
          (res.isDefined, null, res.getOrElse(0L))
        case 3 =>
          localInstance.subsume(TryLockResult(request._2, request._3, request._4))
          noReturnValue
        case 4 =>
          localInstance.unlock()
          noReturnValue
        case 5 => fromTryLockResult(localInstance.spinOnce(request._4))
        case 6 =>
          val res = localInstance.trySubsume(TryLockResult(request._2, request._3, request._4))
          (res.isDefined, res.orNull, 0L)
      }}
      override def createInitializationAndConnectUpdates(value: SubsumableLock, update: (Nothing) => Unit): Unit = Unit
      override def instantiateMirror(value: Unit, request: ((Int, Boolean, SubsumableLock, Long)) => (Boolean, SubsumableLock, Long)): SubsumableLock = new SubsumableLock {
        private def requestWithoutParameters(methodId: Int): (Boolean, SubsumableLock, Long) = request((methodId, false, null, 0L))
        private def requestWithTryLockResultParameter(methodId: Int, param: TryLockResult): (Boolean, SubsumableLock, Long) = request((methodId, param.success, param.newParent, param.globalRoot))

        override def lock(): TryLockResult = toTryLockResult(requestWithoutParameters(0))
        override def tryLock(): TryLockResult = toTryLockResult(requestWithoutParameters(1))
        override def getLockedRoot: Option[SubsumableLock.GUID] = {
          val res = requestWithoutParameters(2)
          if (res._1) Some(res._3) else None
        }
        override def subsume(lockedNewParent: TryLockResult): Unit = requestWithTryLockResultParameter(3, lockedNewParent)
        override def unlock(): Unit = requestWithoutParameters(4)
        override def spinOnce(backoff: Long): TryLockResult = toTryLockResult(request((5, false, null, backoff)))
        override def trySubsume(lockedNewParent: TryLockResult): Option[SubsumableLock] = {
          val res = requestWithTryLockResultParameter(6, lockedNewParent)
          if(res._1) Some(res._2) else None
        }
      }
      override def updateMirror(mirror: SubsumableLock, update: Nothing): Unit = throw new NotImplementedError()
    }
  }
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
