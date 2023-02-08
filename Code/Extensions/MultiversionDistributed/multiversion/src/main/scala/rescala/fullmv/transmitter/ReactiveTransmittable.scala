package rescala.fullmv.transmitter

import loci.transmitter._
import rescala.compat.SignalCompatBundle
import rescala.fullmv.TurnPhase.Type
import rescala.fullmv._
import rescala.fullmv.mirrors._
import rescala.fullmv.sgt.synchronization._
import rescala.fullmv.tasks.TaskBundle
import rescala.operator.{EventBundle, Pulse, SignalBundle}

import java.io.{ByteArrayInputStream, ByteArrayOutputStream, ObjectInputStream, ObjectOutputStream}
import java.util.concurrent.{ConcurrentHashMap, ThreadLocalRandom}
import scala.annotation.{nowarn, tailrec}
import scala.concurrent._
import scala.util.{Failure, Success}

trait ReactiveTransmittableBundle extends FullMVBundle {
  selfType: Mirror with TurnImplBundle with TaskBundle with FullMvStateBundle with SubsumableLockBundle
    with SignalCompatBundle with EventBundle with SignalBundle with ReactiveReflectionBundle
    with ReactiveMirrorBundle with rescala.operator.Operators =>

  object ReactiveTransmittable {
    val DEBUG: Boolean = FullMVUtil.DEBUG || SubsumableLock.DEBUG

    type EndPointWithInfrastructure[T] = Endpoint[MessageWithInfrastructure[T], MessageWithInfrastructure[T]]
    type MessageWithInfrastructure[T]  = (Long, T)

    type Msg[+T] = (
        String,
        List[(
            Host.GUID,
            TurnPhase.Type,
            Option[(CaseClassTransactionSpanningTreeNode[(Host.GUID, TurnPhase.Type)], Int)],
            Option[T]
        )],
        Array[Byte]
    )
    def allEmpty[T](name: String): Msg[T] = (name, Nil, Array.empty)
    val ASYNC_REQUEST                     = 0

    sealed trait Message[+P] {
      def toTuple: Msg[P] =
        this match {
          case UnitResponse  => allEmpty("UnitResponse")
          case Connect(turn) => allEmpty("Connect").copy(_2 = (turn._1, turn._2, Some((turn._3, turn._4)), None) :: Nil)
          case Initialize(initValues, maybeFirstFrame) =>
            allEmpty("Initialize").copy(_2 =
              (maybeFirstFrame match {
                case Some((guid, phase, tree, clock)) => (guid, phase, Some((tree, clock)), None)
                case None                             => (Host.dummyGuid, TurnPhase.Uninitialized, None, None)
              }) :: initValues.map {
                case ((t, p, s, c), v) => (
                    t,
                    p,
                    Some((s, c)): Option[(CaseClassTransactionSpanningTreeNode[TurnPushBundle], Int)],
                    Some(v): Option[P]
                  )
              }
            )
          case AsyncIncrementFrame(turn) =>
            allEmpty("AsyncIncrementFrame").copy(_2 = (turn._1, turn._2, Some((turn._3, turn._4)), None) :: Nil)
          case AsyncIncrementSupersedeFrame(turn, supersede) =>
            allEmpty("AsyncIncrementSupersedeFrame").copy(_2 =
              (turn._1, turn._2, Some((turn._3, turn._4)), None) :: (
                supersede._1,
                supersede._2,
                Some((supersede._3, supersede._4)),
                None
              ) :: Nil
            )
          case AsyncResolveUnchanged(turn) =>
            allEmpty("AsyncResolveUnchanged").copy(_2 = (turn._1, turn._2, Some((turn._3, turn._4)), None) :: Nil)
          case AsyncResolveUnchangedFollowFrame(turn, followFrame) =>
            allEmpty("AsyncResolveUnchangedFollowFrame").copy(_2 =
              (turn._1, turn._2, Some((turn._3, turn._4)), None) :: (
                followFrame._1,
                followFrame._2,
                Some((followFrame._3, followFrame._4)),
                None
              ) :: Nil
            )
          case AsyncNewValue(turn, value) =>
            allEmpty("AsyncNewValue").copy(_2 = (turn._1, turn._2, Some((turn._3, turn._4)), Some(value)) :: Nil)
          case AsyncNewValueFollowFrame(turn, value, followFrame) =>
            allEmpty("AsyncNewValueFollowFrame").copy(_2 =
              (turn._1, turn._2, Some((turn._3, turn._4)), Some(value)) :: (
                followFrame._1,
                followFrame._2,
                Some((followFrame._3, followFrame._4)),
                None
              ) :: Nil
            )
          case AsyncAddPhaseReplicator(turn, knownPhase) =>
            allEmpty("AsyncAddPhaseReplicator").copy(_2 = (turn, knownPhase, None, None) :: Nil)
          case AsyncNewPhase(turn) => allEmpty("AsyncNewPhase").copy(_2 = (turn._1, turn._2, None, None) :: Nil)
          case AsyncAddPredecessorReplicator(turn, startAt, clock) =>
            allEmpty("AddPredecessorReplicator").copy(_2 =
              (turn, TurnPhase.Uninitialized, Some((startAt, clock)), None) :: Nil
            )
          case NewPredecessors(newPredecessors, clock) =>
            allEmpty("NewPredecessors").copy(_2 =
              (Host.dummyGuid, TurnPhase.Uninitialized, Some((newPredecessors, clock)), None) :: Nil
            )
          case AddRemoteBranch(turn, forPhase) =>
            allEmpty("AddRemoteBranch").copy(_2 = (turn, forPhase, None, None) :: Nil)
          case AsyncRemoteBranchComplete(turn, forPhase) =>
            allEmpty("AsyncRemoteBranchComplete").copy(_2 = (turn, forPhase, None, None) :: Nil)
          case AcquireRemoteBranchIfAtMost(turn, phase) =>
            allEmpty("AcquireRemoteBranchIfAtMost").copy(_2 = (turn, phase, None, None) :: Nil)
          case AcquireRemoteBranchResponse(phase) =>
            allEmpty("AcquireRemoteBranchResponse").copy(_2 = (Host.dummyGuid, phase, None, None) :: Nil)
          case AddPredecessor(turn, predecessorTree) =>
            allEmpty("AddPredecessor").copy(_2 =
              (turn, TurnPhase.Uninitialized, Some((predecessorTree, -1)), None) :: Nil
            )
          case BooleanResponse(bool) => allEmpty(if (bool) "BooleanTrueResponse" else "BooleanFalseResponse")
          case MaybeNewReachableSubtree(turn, attachBelow, tree) =>
            allEmpty("MaybeNewReachableSubtree").copy(_2 =
              (turn, TurnPhase.Uninitialized, None, None) :: (
                attachBelow._1,
                attachBelow._2,
                Some((tree, -1)),
                None
              ) :: Nil
            )
          case NewSuccessor(turn, successor) =>
            allEmpty("NewSuccessor").copy(_2 =
              (turn, TurnPhase.Uninitialized, None, None) :: (successor._1, successor._2, None, None) :: Nil
            )
          case TurnGetLockedRoot(turn) =>
            allEmpty("TurnGetLockedRoot").copy(_2 = (turn, TurnPhase.Uninitialized, None, None) :: Nil)
          case LockStateTurnCompletedResponse => allEmpty("LockStateTurnCompletedResponse")
          case LockStateLockedResponse(guid) =>
            allEmpty("LockStateLockedResponse").copy(_2 = (guid, TurnPhase.Uninitialized, None, None) :: Nil)
          case LockStateUnlockedResponse  => allEmpty("LockStateUnlockedResponse")
          case LockStateContendedResponse => allEmpty("LockStateContendedResponse")
          case TurnTryLock(turn) =>
            allEmpty("TurnTryLock").copy(_2 = (turn, TurnPhase.Uninitialized, None, None) :: Nil)
          case TurnLockedResponse(lock) =>
            allEmpty("TurnLockedResponse").copy(_2 = (lock, TurnPhase.Uninitialized, None, None) :: Nil)
          case TurnTrySubsume(turn, lockedNewParent) =>
            allEmpty("TurnTrySubsume").copy(_2 =
              (turn, TurnPhase.Uninitialized, None, None) :: (
                lockedNewParent,
                TurnPhase.Uninitialized,
                None,
                None
              ) :: Nil
            )
          case TurnBlockedResponse     => allEmpty("TurnBlockedResponse")
          case TurnSuccessfulResponse  => allEmpty("TurnSuccessfulResponse")
          case TurnDeallocatedResponse => allEmpty("TurnDeallocatedResponse")
          case LockGetLockedRoot(lock) =>
            allEmpty("LockGetLockedRoot").copy(_2 = (lock, TurnPhase.Uninitialized, None, None) :: Nil)
          case LockTryLock(lock) =>
            allEmpty("LockTryLock").copy(_2 = (lock, TurnPhase.Uninitialized, None, None) :: Nil)
          case LockSuccessfulResponse => allEmpty("LockSuccessfulResponse")
          case LockLockedResponse(newParent) =>
            allEmpty("LockLockedResponse").copy(_2 = (newParent, TurnPhase.Uninitialized, None, None) :: Nil)
          case LockTrySubsume(lock, lockedNewParent) =>
            allEmpty("LockTrySubsume").copy(_2 =
              (lock, TurnPhase.Uninitialized, None, None) :: (
                lockedNewParent,
                TurnPhase.Uninitialized,
                None,
                None
              ) :: Nil
            )
          case LockBlockedResponse(lock) =>
            allEmpty("LockBlockedResponse").copy(_2 = (lock, TurnPhase.Uninitialized, None, None) :: Nil)
          case LockDeallocatedResponse => allEmpty("LockDeallocatedResponse")
          case AsyncLockUnlock(lock) =>
            allEmpty("AsyncLockUnlock").copy(_2 = (lock, TurnPhase.Uninitialized, None, None) :: Nil)
          case LockAsyncRemoteRefDropped(lock) =>
            allEmpty("LockAsyncRemoteRefDropped").copy(_2 = (lock, TurnPhase.Uninitialized, None, None) :: Nil)
          case RemoteExceptionResponse(se) => allEmpty("RemoteExceptionResponse").copy(_3 = se)
        }
    }
    object Message {
      def fromTuple[P](tuple: Msg[P]): Message[P] =
        tuple match {
          case ("UnitResponse", _, _)                                      => UnitResponse
          case ("Connect", (turn, phase, Some((preds, clock)), _) :: _, _) => Connect((turn, phase, preds, clock))
          case ("Initialize", (maybeFirstFrame, maybeFirstPhase, maybeFirstFrameTree, _) :: initValues, _) =>
            Initialize(
              initValues.map {
                case (t, p, Some((s, c)), Some(v)) => ((t, p, s, c), v)
                case _ => throw new IllegalStateException("initializing message from tuple failed")
              },
              if (maybeFirstFrame != Host.dummyGuid)
                Some((maybeFirstFrame, maybeFirstPhase, maybeFirstFrameTree.get._1, maybeFirstFrameTree.get._2))
              else None
            )
          case ("AsyncIncrementFrame", (turn, phase, Some((tree, clock)), _) :: _, _) =>
            AsyncIncrementFrame((turn, phase, tree, clock))
          case (
                "AsyncIncrementSupersedeFrame",
                (turn, phase, Some((tree, clock)), _) :: (fTurn, fPhase, Some((fPredTree, fPredClock)), _) :: _,
                _
              ) => AsyncIncrementSupersedeFrame((turn, phase, tree, clock), (fTurn, fPhase, fPredTree, fPredClock))
          case ("AsyncResolveUnchanged", (turn, phase, Some((tree, clock)), _) :: _, _) =>
            AsyncResolveUnchanged((turn, phase, tree, clock))
          case (
                "AsyncResolveUnchangedFollowFrame",
                (turn, phase, Some((tree, clock)), _) :: (fTurn, fPhase, Some((fPredTree, fPredClock)), _) :: _,
                _
              ) => AsyncResolveUnchangedFollowFrame((turn, phase, tree, clock), (fTurn, fPhase, fPredTree, fPredClock))
          case ("AsyncNewValue", (turn, phase, Some((tree, clock)), Some(value)) :: _, _) =>
            AsyncNewValue((turn, phase, tree, clock), value)
          case (
                "AsyncNewValueFollowFrame",
                (turn, phase, Some((tree, clock)), Some(value)) :: (
                  fTurn,
                  fPhase,
                  Some((fPredTree, fPredClock)),
                  _
                ) :: _,
                _
              ) => AsyncNewValueFollowFrame((turn, phase, tree, clock), value, (fTurn, fPhase, fPredTree, fPredClock))
          case ("AsyncAddPhaseReplicator", (turn, knownPhase, _, _) :: _, _) =>
            AsyncAddPhaseReplicator(turn, knownPhase)
          case ("AsyncNewPhase", (turn, phase, _, _) :: _, _) => AsyncNewPhase(turn -> phase)
          case ("AddPredecessorReplicator", (turn, _, Some((tree, clock)), _) :: _, _) =>
            AsyncAddPredecessorReplicator(turn, tree, clock)
          case ("NewPredecessors", (_, _, Some((tree, clock)), _) :: _, _) => NewPredecessors(tree, clock)
          case ("AddRemoteBranch", (turn, phase, _, _) :: _, _) =>
            AddRemoteBranch(turn, phase) // TODO this should not be an assert, but a push phase?
          case ("AsyncRemoteBranchComplete", (turn, phase, _, _) :: _, _)   => AsyncRemoteBranchComplete(turn, phase)
          case ("AcquireRemoteBranchIfAtMost", (turn, phase, _, _) :: _, _) => AcquireRemoteBranchIfAtMost(turn, phase)
          case ("AcquireRemoteBranchResponse", (_, phase, _, _) :: _, _)    => AcquireRemoteBranchResponse(phase)
          case ("AddPredecessor", (turn, _, Some((tree, _)), _) :: _, _)    => AddPredecessor(turn, tree)
          case ("BooleanTrueResponse", _, _)                                => BooleanResponse(true)
          case ("BooleanFalseResponse", _, _)                               => BooleanResponse(false)
          case (
                "MaybeNewReachableSubtree",
                (turn, _, _, _) :: (attachBelow, attachPhase, Some((tree, _)), _) :: _,
                _
              ) =>
            MaybeNewReachableSubtree(turn, attachBelow -> attachPhase, tree)
          case ("NewSuccessor", (turn, _, _, _) :: (successor, successorPhase, _, _) :: _, _) =>
            NewSuccessor(turn, successor -> successorPhase)
          case ("TurnGetLockedRoot", (turn, _, _, _) :: _, _)                      => TurnGetLockedRoot(turn)
          case ("LockStateTurnCompletedResponse", _, _)                            => LockStateTurnCompletedResponse
          case ("LockStateLockedResponse", (guid, _, _, _) :: _, _)                => LockStateLockedResponse(guid)
          case ("LockStateUnlockedResponse", _, _)                                 => LockStateUnlockedResponse
          case ("LockStateContendedResponse", _, _)                                => LockStateContendedResponse
          case ("TurnTryLock", (turn, _, _, _) :: _, _)                            => TurnTryLock(turn)
          case ("TurnLockedResponse", (lock, _, _, _) :: _, _)                     => TurnLockedResponse(lock)
          case ("TurnTrySubsume", (turn, _, _, _) :: (lock, _, _, _) :: _, _)      => TurnTrySubsume(turn, lock)
          case ("TurnBlockedResponse", _, _)                                       => TurnBlockedResponse
          case ("TurnSuccessfulResponse", _, _)                                    => TurnSuccessfulResponse
          case ("TurnDeallocatedResponse", _, _)                                   => TurnDeallocatedResponse
          case ("LockGetLockedRoot", (lock, _, _, _) :: _, _)                      => LockGetLockedRoot(lock)
          case ("LockTryLock", (lock, _, _, _) :: _, _)                            => LockTryLock(lock)
          case ("LockSuccessfulResponse", _, _)                                    => LockSuccessfulResponse
          case ("LockLockedResponse", (newParent, _, _, _) :: _, _)                => LockLockedResponse(newParent)
          case ("LockTrySubsume", (lock, _, _, _) :: (newParent, _, _, _) :: _, _) => LockTrySubsume(lock, newParent)
          case ("LockBlockedResponse", (newParent, _, _, _) :: _, _)               => LockBlockedResponse(newParent)
          case ("LockDeallocatedResponse", _, _)                                   => LockDeallocatedResponse
          case ("AsyncLockUnlock", (lock, _, _, _) :: _, _)                        => AsyncLockUnlock(lock)
          case ("LockAsyncRemoteRefDropped", (lock, _, _, _) :: _, _)              => LockAsyncRemoteRefDropped(lock)
          case ("RemoteExceptionResponse", _, se)                                  => RemoteExceptionResponse(se)
          case otherwise =>
            val e = new AssertionError("Unrecognized message: " + otherwise)
            e.printStackTrace()
            throw e
        }
    }
    sealed trait PossiblyBlockingTopLevel[+P] extends Message[P]
    sealed trait UnderlyingChatter            extends Message[Nothing]

    sealed trait Async[+P]                         extends Message[P]
    sealed trait PossiblyBlockingTopLevelAsync[+P] extends Async[P] with PossiblyBlockingTopLevel[P]
    sealed trait UnderlyingChatterAsync            extends Async[Nothing] with UnderlyingChatter

    sealed trait Request[+P] extends Message[P] {
      type Response <: ReactiveTransmittable.this.Response[P]
    }
    sealed trait PossiblyBlockingTopLevelRequest[+P] extends Request[P] with PossiblyBlockingTopLevel[P]
    sealed trait UnderlyingChatterRequest            extends Request[Nothing] with UnderlyingChatter

    sealed trait Response[+P]                                            extends Message[P]
    case class RemoteExceptionResponse(serializedThrowable: Array[Byte]) extends Response[Nothing]
    case object UnitResponse                                             extends Response[Nothing]

    implicit def unitResponseToUnitFuture(future: Future[UnitResponse.type]): Future[Unit] =
      future.map(_ => ())(FullMVUtil.notWorthToMoveToTaskpool)
    implicit def unitToUnitResponseFuture(future: Future[Unit]): Future[UnitResponse.type] =
      future.map(_ => UnitResponse)(FullMVUtil.notWorthToMoveToTaskpool)

    type TurnPushBundle         = (Host.GUID, TurnPhase.Type)
    type TopLevelTurnPushBundle = (Host.GUID, TurnPhase.Type, CaseClassTransactionSpanningTreeNode[TurnPushBundle], Int)

    /** Connection Establishment * */
    case class Connect[P](turn: TopLevelTurnPushBundle) extends PossiblyBlockingTopLevelRequest[P] {
      override type Response = Initialize[P]
    }
    case class Initialize[P](
        initValues: List[(TopLevelTurnPushBundle, P)],
        maybeFirstFrame: Option[TopLevelTurnPushBundle]
    ) extends Response[P]

    /** [[ReactiveReflectionProxy]] * */
    case class AsyncIncrementFrame(turn: TopLevelTurnPushBundle) extends PossiblyBlockingTopLevelAsync[Nothing]
    case class AsyncIncrementSupersedeFrame(turn: TopLevelTurnPushBundle, supersede: TopLevelTurnPushBundle)
        extends PossiblyBlockingTopLevelAsync[Nothing]
    case class AsyncResolveUnchanged(turn: TopLevelTurnPushBundle) extends PossiblyBlockingTopLevelAsync[Nothing]
    case class AsyncResolveUnchangedFollowFrame(turn: TopLevelTurnPushBundle, followFrame: TopLevelTurnPushBundle)
        extends PossiblyBlockingTopLevelAsync[Nothing]
    case class AsyncNewValue[P](turn: TopLevelTurnPushBundle, value: P) extends PossiblyBlockingTopLevelAsync[P]
    case class AsyncNewValueFollowFrame[P](turn: TopLevelTurnPushBundle, value: P, followFrame: TopLevelTurnPushBundle)
        extends PossiblyBlockingTopLevelAsync[P]

    /** [[FullMVTurnProxy]] * */
    case class AsyncAddPhaseReplicator(turn: Host.GUID, alreadyKnownPhase: TurnPhase.Type)
        extends UnderlyingChatterAsync
    case class AsyncAddPredecessorReplicator(
        turn: Host.GUID,
        startAt: CaseClassTransactionSpanningTreeNode[TurnPushBundle],
        clock: Int
    ) extends UnderlyingChatterAsync
    case class AddRemoteBranch(turn: Host.GUID, forPhase: TurnPhase.Type) extends UnderlyingChatterRequest {
      override type Response = UnitResponse.type
    }
    case class AsyncRemoteBranchComplete(turn: Host.GUID, forPhase: TurnPhase.Type) extends UnderlyingChatterAsync
    case class AcquireRemoteBranchIfAtMost(turn: Host.GUID, phase: TurnPhase.Type) extends UnderlyingChatterRequest {
      override type Response = AcquireRemoteBranchResponse
    }
    case class AcquireRemoteBranchResponse(phase: TurnPhase.Type) extends Response[Nothing]
    case class AddPredecessor(turn: Host.GUID, predecessorTree: CaseClassTransactionSpanningTreeNode[TurnPushBundle])
        extends UnderlyingChatterRequest {
      override type Response = BooleanResponse
    }
    case class BooleanResponse(bool: Boolean) extends Response[Nothing]
    case class MaybeNewReachableSubtree(
        turn: Host.GUID,
        attachBelow: TurnPushBundle,
        tree: CaseClassTransactionSpanningTreeNode[TurnPushBundle]
    ) extends UnderlyingChatterRequest {
      override type Response = UnitResponse.type
    }
    case class NewSuccessor(turn: Host.GUID, successor: TurnPushBundle) extends UnderlyingChatterRequest {
      override type Response = UnitResponse.type
    }

    /** [[SubsumableLockEntryPoint]] * */
    case class TurnGetLockedRoot(turn: Host.GUID) extends UnderlyingChatterRequest {
      override type Response = TurnLockStateResponse
    }
    sealed trait TurnLockStateResponse         extends Response[Nothing]
    case object LockStateTurnCompletedResponse extends TurnLockStateResponse
    case class TurnTryLock(turn: Host.GUID) extends UnderlyingChatterRequest {
      override type Response = TurnTryLockResponse
    }
    sealed trait TurnTryLockResponse               extends Response[Nothing]
    case class TurnLockedResponse(lock: Host.GUID) extends TurnTryLockResponse
    case class TurnTrySubsume(turn: Host.GUID, lockedNewParent: Host.GUID) extends UnderlyingChatterRequest {
      override type Response = TurnTrySubsumeResponse
    }
    sealed trait TurnTrySubsumeResponse extends Response[Nothing]
    case object TurnSuccessfulResponse  extends TurnTrySubsumeResponse
    case object TurnBlockedResponse     extends TurnTryLockResponse with TurnTrySubsumeResponse
    case object TurnDeallocatedResponse extends TurnTryLockResponse with TurnTrySubsumeResponse

    /** [[SubsumableLockProxy]] * */
    case class LockGetLockedRoot(lock: Host.GUID) extends UnderlyingChatterRequest {
      override type Response = LockStateResponse
    }
    sealed trait LockStateResponse                      extends Response[Nothing]
    case class LockStateLockedResponse(guid: Host.GUID) extends LockStateResponse with TurnLockStateResponse
    case object LockStateUnlockedResponse               extends LockStateResponse with TurnLockStateResponse
    case object LockStateContendedResponse              extends LockStateResponse
    case class LockTryLock(lock: Host.GUID) extends UnderlyingChatterRequest {
      override type Response = LockTryLockResponse
    }
    sealed trait LockTryLockResponse               extends Response[Nothing]
    case class LockLockedResponse(lock: Host.GUID) extends LockTryLockResponse
    case class LockTrySubsume(lock: Host.GUID, lockedNewParent: Host.GUID) extends UnderlyingChatterRequest {
      override type Response = LockTrySubsumeResponse
    }
    sealed trait LockTrySubsumeResponse                   extends Response[Nothing]
    case object LockSuccessfulResponse                    extends LockTrySubsumeResponse
    case class LockBlockedResponse(lock: Host.GUID)       extends LockTryLockResponse with LockTrySubsumeResponse
    case object LockDeallocatedResponse                   extends LockTryLockResponse with LockTrySubsumeResponse
    case class AsyncLockUnlock(lock: Host.GUID)           extends UnderlyingChatterAsync
    case class LockAsyncRemoteRefDropped(lock: Host.GUID) extends UnderlyingChatterAsync

    /** [[FullMVTurnPhaseReflectionProxy]] * */
    case class AsyncNewPhase(turn: TurnPushBundle) extends UnderlyingChatterAsync

    /** [[FullMVTurnPredecessorReflectionProxy]] * */
    case class NewPredecessors(newPredecessors: CaseClassTransactionSpanningTreeNode[TurnPushBundle], clock: Int)
        extends UnderlyingChatterRequest {
      override type Response = UnitResponse.type
    }

    def deserializeThrowable(se: Array[Byte]): Throwable = {
      val bais = new ByteArrayInputStream(se)
      val ois  = new ObjectInputStream(bais)
      ois.readObject().asInstanceOf[Throwable]
    }
    def serializeThrowable(t: Throwable): Array[Byte] = {
      val baos = new ByteArrayOutputStream()
      val oos  = new ObjectOutputStream(baos)
      oos.writeObject(t)
      oos.flush()
      baos.toByteArray
    }
    sealed trait Pluse[+P] {
      @nowarn
      def toPulse: Pulse[P] =
        this match {
          case Pluse.NoChange        => Pulse.NoChange
          case Pluse.Value(v)        => Pulse.Value(v)
          case Pluse.Exceptional(se) => Pulse.Exceptional(deserializeThrowable(se))
        }
    }
    object Pluse {
      def fromPulse[P](pulse: Pulse[P]): Pluse[P] = {
        pulse match {
          case Pulse.NoChange       => Pluse.NoChange
          case Pulse.Value(v)       => Pluse.Value(v)
          case Pulse.Exceptional(t) => Pluse.Exceptional(serializeThrowable(t))
        }
      }
      case object NoChange extends Pluse[Nothing]
      @nowarn
      final case class Value[+P](update: P) extends Pluse[P]
      @nowarn
      final case class Exceptional(serializedThrowable: Array[Byte]) extends Pluse[Nothing]
    }

    @nowarn
    implicit def fullvmPluseTransmittable[T, I, U](implicit
        transmittable: Transmittable[(Option[T], Option[Array[Byte]]), I, (Option[U], Option[Array[Byte]])]
    ): DelegatingTransmittable[Pluse[T], I, Pluse[U]] {
      type Delegates = transmittable.Type
    } =
      DelegatingTransmittable(
        provide = (value, context) =>
          context.delegate(value match {
            case Pluse.Value(v)       => Some(v) -> None
            case Pluse.Exceptional(e) => None    -> Some(e)
            case _                    => None    -> None
          }),
        receive = (value, context) =>
          context.delegate(value) match {
            case (Some(v), _) => Pluse.Value(v)
            case (_, Some(e)) => Pluse.Exceptional(e)
            case _            => Pluse.NoChange
          }
      )

    implicit def fullvmCaseClassTransactionSpanningTreeNodeTransmittable
        : IdenticallyTransmittable[rescala.fullmv.CaseClassTransactionSpanningTreeNode[(Long, Int)]] =
      IdenticallyTransmittable()

    implicit def fullmvSignalTransmittable[T, I](implicit
        host: FullMVEngine,
        transmittable: Transmittable[
          MessageWithInfrastructure[Msg[Pluse[T]]],
          I,
          MessageWithInfrastructure[Msg[Pluse[T]]]
        ]
    ): ConnectedTransmittable[Signal[T], I, Signal[T]] {
      type Message = transmittable.Type
    } =
      ConnectedTransmittable(
        provide = (value, context) => {
          val signal = new DistributedSignal[T, I](Pulse.empty)
          signal.send(value, context.endpoint)
        },
        receive = (value, context) => {
          val signal = new DistributedSignal[T, I](Pulse.empty)
          signal.receive(value, context.endpoint)
        }
      )

    implicit def fullmvEventTransmittable[T, I](implicit
        host: FullMVEngine,
        transmittable: Transmittable[
          MessageWithInfrastructure[Msg[Pluse[T]]],
          I,
          MessageWithInfrastructure[Msg[Pluse[T]]]
        ]
    ): ConnectedTransmittable[Event[T], I, Event[T]] {
      type Message = transmittable.Type
    } =
      ConnectedTransmittable(
        provide = (value, context) => {
          val event = new DistributedEvent[T, I](Pulse.empty)
          event.send(value, context.endpoint)
        },
        receive = (value, context) => {
          val event = new DistributedEvent[T, I](Pulse.empty)
          event.receive(value, context.endpoint)
        }
      )
  }

  abstract class DistributedReactive[P, R[_] <: ReSource, I](implicit
      val host: FullMVEngine
  ) {
    def topLevelBundle(turn: FullMVTurn): ReactiveTransmittable.TopLevelTurnPushBundle = {
      assert(turn.host == host, s"$turn is not on $host?!")
      val startReplication = turn.clockedPredecessors
      (turn.guid, turn.phase, startReplication._1.map(bundle), startReplication._2)
    }
    def bundle(turn: FullMVTurn): ReactiveTransmittable.TurnPushBundle = {
      assert(turn.host == host, s"$turn is not on $host?!")
      (turn.guid, turn.phase)
    }

    import ReactiveTransmittable._

    type Msg      = ReactiveTransmittable.Msg[Pluse[P]]
    type Message  = ReactiveTransmittable.Message[Pluse[P]]
    type Response = ReactiveTransmittable.Response[Pluse[P]]

    val requestTracker = new ConcurrentHashMap[Long, Promise[_ <: Response]]()

    def handleChatter(endpoint: EndPointWithInfrastructure[Msg], requestId: Long, message: UnderlyingChatter): Unit = {
      message match {
        case async: UnderlyingChatterAsync =>
          if (ReactiveTransmittable.DEBUG) println(s"[${Thread.currentThread().getName}] $host processing async $async")
          try {
            handleAsyncChatter(endpoint, async)
          } catch {
            // TODO cannot propagate this back to sender because async, what else to do?
            case t: Throwable => new Exception(s"Error on $host processing async $async", t).printStackTrace()
          }
        case request: UnderlyingChatterRequest =>
          if (ReactiveTransmittable.DEBUG)
            println(s"[${Thread.currentThread().getName}] $host processing request $request")
          try {
            handleRequestChatter(endpoint, request).onComplete {
              case Success(response) =>
                if (ReactiveTransmittable.DEBUG)
                  println(s"[${Thread.currentThread().getName}] $host replying $requestId: $response")
                endpoint.send((requestId, response.toTuple))
              case Failure(t) =>
                new Exception(
                  s"[${Thread.currentThread().getName}] $host request $requestId completed, but resulted in failure; returning ${t.getClass.getName}: ${t.getMessage} to remote",
                  t
                ).printStackTrace()
                endpoint.send(requestId -> RemoteExceptionResponse(serializeThrowable(t)).toTuple)
            }(FullMVUtil.notWorthToMoveToTaskpool)
          } catch {
            case t: Throwable =>
              new Exception(
                s"[${Thread.currentThread().getName}] $host request $requestId failed to execute; returning ${t.getClass.getName}: ${t.getMessage} to remote",
                t
              ).printStackTrace()
              endpoint.send(requestId -> RemoteExceptionResponse(serializeThrowable(t)).toTuple)
          }
      }
    }

    def consumeResponse(requestId: Long, response: Response): Unit = {
      try {
        val promise = requestTracker.remove(requestId).asInstanceOf[Promise[Response]] /* typesafety yay */
        assert(promise != null, s"request $requestId unknown!")
        promise.complete(response match {
          case RemoteExceptionResponse(se) =>
            val t = deserializeThrowable(se)
            new Exception(s"$host received exception response for $requestId", t).printStackTrace()
            Failure(t)
          case otherwise => Success(response)
        })
      } catch {
        // this should only happen on framework bugs. Since we're just completing a future, everything client-code that happens as
        // a reaction of that should catch and propagate exceptions itself, so no client exceptions should be thrown uncaught..
        case t: Throwable => t.printStackTrace()
      }
    }

    def send(
        reactive: R[P],
        endpoint: EndPointWithInfrastructure[Msg]
    ): MessageWithInfrastructure[Msg] = {
      if (ReactiveTransmittable.DEBUG) println(s"[${Thread.currentThread().getName}] $host sending $reactive")
      endpoint.receive.foreach { (mwi: MessageWithInfrastructure[Msg]) =>
        if (ReactiveTransmittable.DEBUG)
          println(s"[${Thread.currentThread().getName}] sender $host receive incoming $mwi")
        val (requestId: Long, msg: Msg) = mwi
        Message.fromTuple(msg) match {
          case response: Response =>
            consumeResponse(requestId, response)
          case Connect(bundle) =>
            host.threadPool.submit(new Runnable {
              override def run(): Unit = {
                try {
                  if (ReactiveTransmittable.DEBUG)
                    println(s"[${Thread.currentThread().getName}] $host processing connect $reactive $bundle")
                  val (initValues, maybeFirstFrame) = ReactiveMirror[Pulse[P]](
                    reactive,
                    lookUpLocalTurnParameterInstance(bundle, endpoint),
                    isTransient,
                    s"Mirror($endpoint)"
                  )(toPulse(reactive), new ReactiveReflectionProxyToEndpoint(endpoint))
                  val response = Initialize(
                    initValues.map {
                      case (aTurn, value) =>
                        (topLevelBundle(aTurn), Pluse.fromPulse(value))
                    },
                    maybeFirstFrame.map(topLevelBundle)
                  )
                  endpoint.send(requestId -> response.toTuple)
                } catch {
                  case t: Throwable =>
                    new Exception(
                      s"[${Thread.currentThread().getName}] $host request $requestId failed to execute; returning ${t.getClass.getName}: ${t.getMessage} to remote",
                      t
                    ).printStackTrace()
                    endpoint.send(requestId -> RemoteExceptionResponse(serializeThrowable(t)).toTuple)
                }
              }
            })
          case otherwise: PossiblyBlockingTopLevel[_] =>
            new Exception("Illegal top level message received on sender side: " + otherwise).printStackTrace()
          case otherwise: UnderlyingChatter =>
            handleChatter(endpoint, requestId, otherwise)
        }
      }
      (0L, allEmpty(reactive.toString))
    }

    def doAsync(endpoint: EndPointWithInfrastructure[Msg], parameters: Async[Pluse[P]]): Unit = {
      if (ReactiveTransmittable.DEBUG) println(s"[${Thread.currentThread().getName}] $host send async $parameters")
      endpoint.send((0L, parameters.toTuple))
    }

    def doRequest(
        endpoint: EndPointWithInfrastructure[Msg],
        parameters: Request[Pluse[P]]
    ): Future[parameters.Response] = {
      val promise = Promise[parameters.Response]()

      @inline
      @tailrec def createRequest(): Long = {
        val requestId = ThreadLocalRandom.current().nextLong()
        if (requestId != 0 && requestTracker.putIfAbsent(requestId, promise) == null) requestId else createRequest()
      }

      val requestId = createRequest()
      if (ReactiveTransmittable.DEBUG)
        println(s"[${Thread.currentThread().getName}] $host send request $requestId: $parameters")
      endpoint.send((requestId, parameters.toTuple))
      promise.future
    }

    def lookUpLocalLockParameterInstanceWithReference(
        guid: Host.GUID,
        endpoint: EndPointWithInfrastructure[Msg]
    ): SubsumableLock = {
      host.lockHost.getCachedOrReceiveRemoteWithReference(guid, new SubsumableLockProxyToEndpoint(guid, endpoint))
    }

    def localLockReceiverInstance(guid: Host.GUID): Option[SubsumableLockProxy] = {
      val instance = host.lockHost.getInstance(guid)
      if (ReactiveTransmittable.DEBUG) {
        if (instance.isDefined) {
          println(s"[${Thread.currentThread().getName}] $host retrieved chached receiver $instance")
        } else {
          println(
            s"[${Thread.currentThread().getName}] $host receiver lock lookup failed for $guid, should have been concurrently gc'd."
          )
        }
      }
      instance
    }

    def getKnownLocalTurnParameterInstance(
        bundle: TurnPushBundle,
    ): FullMVTurn = {
      val turn = host.getInstance(bundle._1).get
      if (turn.isInstanceOf[FullMVTurnReflection]) turn.asInstanceOf[FullMVTurnReflection].asyncNewPhase(bundle._2)
      turn
    }

    def lookUpLocalTurnParameterInstance(
        bundle: TopLevelTurnPushBundle,
        endpoint: EndPointWithInfrastructure[Msg]
    ): FullMVTurn = {
      val res = lookUpLocalTurnParameterInstance(bundle._1 -> bundle._2, endpoint)
      res.ensurePredecessorReplication(bundle._3.map(lookUpLocalTurnParameterInstance(_, endpoint)), bundle._4)
      res
    }
    def lookUpLocalTurnParameterInstance(
        bundle: TurnPushBundle,
        endpoint: EndPointWithInfrastructure[Msg]
    ): FullMVTurn = {
      val (guid, phase) = bundle
      val active        = phase < TurnPhase.Completed
      if (active) {
        host.getCachedOrReceiveRemote(
          guid,
          new FullMVTurnReflection(host, guid, phase, new FullMVTurnMirrorProxyToEndpoint(guid, endpoint))
        ) match {
          case Instantiated(reflection) =>
            reflection.asyncNewPhase(phase)
            doAsync(endpoint, AsyncAddPhaseReplicator(guid, phase))
            reflection
          case Found(reflection: FullMVTurnReflection) =>
            reflection.asyncNewPhase(phase)
            reflection
          case Found(notReflection) =>
            assert(
              phase <= notReflection.phase,
              s"apparently $notReflection has a newer phase ($phase) on a remote copy than the local original?"
            )
            notReflection
          case _ => throw IllegalStateException("should be unreachable, but compiler is unsure")
        }
      } else {
        new FullMVTurnReflection(host, guid, phase, null)
      }
    }

    def localTurnReceiverInstance(guid: Host.GUID): Option[FullMVTurn] = {
      host.getInstance(guid)
    }

    def localTurnReflectionReceiverInstance(guid: TurnPushBundle): Option[FullMVTurnReflection] = {
      localTurnReceiverInstance(guid._1).map { instance =>
        val reflection = instance.asInstanceOf[FullMVTurnReflection]
        reflection.asyncNewPhase(guid._2)
        reflection
      }
    }

    val valuePersistency: Pulse[P]

    val ignitionRequiresReevaluation: Boolean
    val isTransient: Boolean
    def toPulse(reactive: R[P]): reactive.Value => Pulse[P]

    def receive(
        value: MessageWithInfrastructure[Msg],
        endpoint: EndPointWithInfrastructure[Msg]
    ): R[P] = {
      if (ReactiveTransmittable.DEBUG) println(s"[${Thread.currentThread().getName}] $host receiving a value")
      val turn = host.newTurn()
      turn.beginExecuting()
      val state      = turn.makeDerivedStructState[Pulse[P]](valuePersistency)
      val reflection = instantiate(state, turn, value._2._1)
      if (ReactiveTransmittable.DEBUG)
        println(
          s"[${Thread.currentThread().getName}] $host instantiating $reflection, requesting initialization starting at $turn"
        )
      endpoint.receive.foreach { (mwi: MessageWithInfrastructure[Msg]) =>
        if (ReactiveTransmittable.DEBUG)
          println(s"[${Thread.currentThread().getName}] receiver $host receive incoming $mwi")
        val (requestId: Long, msg: Msg) = mwi
        Message.fromTuple(msg) match {
          case response: Response =>
            consumeResponse(requestId, response)
          case topLevel: PossiblyBlockingTopLevelAsync[Pluse[P]] =>
            if (ReactiveTransmittable.DEBUG)
              println(s"[${Thread.currentThread().getName}] $host processing top level async $topLevel for $reflection")
            try {
              handleTopLevelAsync(reflection, endpoint, topLevel)
            } catch {
              // TODO cannot propagate this back to sender because async, what else to do?
              case t: Throwable =>
                new Exception(s"$host error processing top level async $topLevel for $reflection", t).printStackTrace()
            }
          case otherwise: PossiblyBlockingTopLevel[_] =>
            new Exception("Illegal top level message received on receiver side: " + otherwise).printStackTrace()
          case otherwise: UnderlyingChatter =>
            handleChatter(endpoint, requestId, otherwise)
        }
      }
      doRequest(endpoint, Connect[Pluse[P]](topLevelBundle(turn))).onComplete {
        case Success(Initialize(initValues, maybeFirstFrame)) =>
          try {
            if (ReactiveTransmittable.DEBUG)
              println(s"[${Thread.currentThread().getName}] $host received initialization package for $reflection")
            val reflectionInitValues = initValues.map {
              case (mirrorTurn, v) =>
                val turn = lookUpLocalTurnParameterInstance(mirrorTurn, endpoint)
                turn -> v
            }
            val reflectionMaybeFirstFrame = maybeFirstFrame.map { mirrorTurn =>
              lookUpLocalTurnParameterInstance(mirrorTurn, endpoint)
            }

            state.retrofitSinkFrames(reflectionInitValues.map(_._1), reflectionMaybeFirstFrame, +1).foreach(
              _.activeBranchDifferential(TurnPhase.Executing, 1)
            )
            for ((reflectionTurn, v) <- reflectionInitValues) reflection.buffer(reflectionTurn, v.toPulse)

            turn.initialize(reflection, Set.empty, ignitionRequiresReevaluation)

            turn.completeExecuting()
          } catch {
            case t: Throwable =>
              new Exception(
                s"$host processing initialization package for $reflection from remote failed",
                t
              ).printStackTrace()
          }
        case Failure(throwable) =>
          new Exception(s"$host remote connect request for $reflection failed", throwable).printStackTrace()
      }(host.threadPool)

      reflection
    }

    def instantiate(
        state: FullMVState[Pulse[P], FullMVTurn],
        initTurn: FullMVTurn,
        name: String
    ): ReactiveReflectionImpl[Pulse[P]] with R[P]

    def handleTopLevelAsync(
        reflection: ReactiveReflection[Pulse[P]],
        endpoint: EndPointWithInfrastructure[Msg],
        message: PossiblyBlockingTopLevelAsync[Pluse[P]]
    ): Unit =
      message match {
        case AsyncIncrementFrame(turn) =>
          reflection.asyncIncrementFrame(lookUpLocalTurnParameterInstance(turn, endpoint))
        case AsyncIncrementSupersedeFrame(turn, supersede) =>
          reflection.asyncIncrementSupersedeFrame(
            lookUpLocalTurnParameterInstance(turn, endpoint),
            lookUpLocalTurnParameterInstance(supersede, endpoint)
          )
        case AsyncResolveUnchanged(turn) =>
          reflection.asyncResolvedUnchanged(lookUpLocalTurnParameterInstance(turn, endpoint))
        case AsyncResolveUnchangedFollowFrame(turn, followFrame) =>
          reflection.asyncResolvedUnchangedFollowFrame(
            lookUpLocalTurnParameterInstance(turn, endpoint),
            lookUpLocalTurnParameterInstance(followFrame, endpoint)
          )
        case AsyncNewValue(turn, value) =>
          reflection.asyncNewValue(lookUpLocalTurnParameterInstance(turn, endpoint), value.toPulse)
        case AsyncNewValueFollowFrame(turn, value, followFrame) =>
          reflection.asyncNewValueFollowFrame(
            lookUpLocalTurnParameterInstance(turn, endpoint),
            value.toPulse,
            lookUpLocalTurnParameterInstance(followFrame, endpoint)
          )
      }

    def handleAsyncChatter(endpoint: EndPointWithInfrastructure[Msg], message: UnderlyingChatterAsync): Unit =
      message match {
        case AsyncRemoteBranchComplete(receiver, forPhase) =>
          val maybeTurn = localTurnReceiverInstance(receiver)
          assert(
            maybeTurn.isDefined,
            s"supposedly a remote still has a branch, but $maybeTurn has already been deallocated"
          )
          maybeTurn.get.asyncRemoteBranchComplete(forPhase)
        case AsyncLockUnlock(receiver) =>
          val lock = localLockReceiverInstance(receiver)
          assert(
            lock.isDefined,
            s"unlock should only be called along paths on which a reference is held, so concurrent deallocation should be impossible."
          )
          lock.get.remoteUnlock()
        case LockAsyncRemoteRefDropped(receiver) =>
          val lock = localLockReceiverInstance(receiver)
          assert(
            lock.isDefined,
            s"a reference should only be dropped if it currently is held, so concurrent deallocation should be impossible."
          )
          lock.get.asyncRemoteRefDropped()

        case AsyncAddPhaseReplicator(receiver, known) =>
          localTurnReceiverInstance(receiver) match {
            case Some(turn) =>
              turn.asyncAddPhaseReplicator(new FullMVTurnPhaseReflectionProxyToEndpoint(turn, endpoint), known)
            case None =>
              if (ReactiveTransmittable.DEBUG)
                println(s"[${Thread.currentThread().getName}] $host receiver of request $message was deallocated")
              doAsync(endpoint, AsyncNewPhase(receiver -> TurnPhase.Completed))
          }
        case AsyncNewPhase(bundle) =>
          localTurnReflectionReceiverInstance(bundle)
          ()
        case AsyncAddPredecessorReplicator(receiver, startAt, clock) =>
          localTurnReceiverInstance(receiver) match {
            case Some(turn) =>
              turn.asyncAddPredecessorReplicator(
                new FullMVTurnPredecessorReflectionProxyToEndpoint(turn, endpoint),
                startAt.map(lookUpLocalTurnParameterInstance(_, endpoint)),
                clock
              )
            case None =>
              if (ReactiveTransmittable.DEBUG)
                println(
                  s"[${Thread.currentThread().getName}] $host receiver of request $message was deallocated; ignoring call as it is no longer needed"
                )
          }
      }

    def handleRequestChatter(
        endpoint: EndPointWithInfrastructure[Msg],
        request: UnderlyingChatterRequest
    ): Future[Response] =
      request match {
        case AddRemoteBranch(receiver, forPhase) =>
          val maybeTurn = localTurnReceiverInstance(receiver)
          assert(
            maybeTurn.isDefined,
            s"someone tried to revive $receiver, which should thus not have been possible to be deallocated"
          )
          maybeTurn.get.addRemoteBranch(forPhase)
        case AcquireRemoteBranchIfAtMost(receiver, phase) =>
          localTurnReceiverInstance(receiver) match {
            case Some(turn) =>
              turn.acquireRemoteBranchIfPhaseAtMost(phase).map(AcquireRemoteBranchResponse.apply)(
                FullMVUtil.notWorthToMoveToTaskpool
              )
            case None =>
              if (ReactiveTransmittable.DEBUG)
                println(s"[${Thread.currentThread().getName}] $host receiver of request $request was deallocated")
              Future.successful(AcquireRemoteBranchResponse(TurnPhase.Completed))
          }
        case MaybeNewReachableSubtree(receiver, attachBelow, tree) =>
          val maybeTurn = localTurnReceiverInstance(receiver)
          assert(
            maybeTurn.isDefined,
            s"someone tried to share possible transitive predecessors with $receiver, which should thus not have been possible to be deallocated"
          )
          maybeTurn.get.maybeNewReachableSubtree(
            getKnownLocalTurnParameterInstance(attachBelow),
            tree.map { lookUpLocalTurnParameterInstance(_, endpoint) }
          )
        case AddPredecessor(receiver, predecessorTree) =>
          val maybeTurn = localTurnReceiverInstance(receiver)
          assert(
            maybeTurn.isDefined,
            s"someone tried to add predecessors on turn $receiver, which should thus not have been possible to be deallocated"
          )
          maybeTurn.get.addPredecessor(predecessorTree.map { lookUpLocalTurnParameterInstance(_, endpoint) }).map(
            BooleanResponse.apply
          )(FullMVUtil.notWorthToMoveToTaskpool)
        case NewSuccessor(receiver, successor) =>
          localTurnReceiverInstance(receiver) match {
            case Some(turn) => turn.newSuccessor(lookUpLocalTurnParameterInstance(successor, endpoint))
            case None =>
              if (ReactiveTransmittable.DEBUG)
                println(
                  s"[${Thread.currentThread().getName}] $host receiver of request $request was deallocated; ignoring call as it is no longer needed"
                )
              Future.successful(UnitResponse)
          }
        case TurnGetLockedRoot(receiver) =>
          localTurnReceiverInstance(receiver) match {
            case Some(turn) => turn.getLockedRoot.map {
                case LockedState(guid) => LockStateLockedResponse(guid)
                case UnlockedState     => LockStateUnlockedResponse
                case CompletedState    => LockStateTurnCompletedResponse
              }(FullMVUtil.notWorthToMoveToTaskpool)
            case None =>
              if (ReactiveTransmittable.DEBUG)
                println(s"[${Thread.currentThread().getName}] $host receiver of request $request was deallocated")
              Future.successful(LockStateTurnCompletedResponse)
          }
        case TurnTryLock(receiver) =>
          localTurnReceiverInstance(receiver) match {
            case Some(turn) => turn.remoteTryLock().map {
                case Locked(lock) =>
                  assert(lock.host == host.lockHost, s"$lock is not on ${host.lockHost}?!")
                  TurnLockedResponse(lock.guid)
                case Blocked     => TurnBlockedResponse
                case Deallocated => TurnDeallocatedResponse
              }(FullMVUtil.notWorthToMoveToTaskpool)
            case None =>
              if (ReactiveTransmittable.DEBUG)
                println(s"[${Thread.currentThread().getName}] $host receiver of request $request was deallocated")
              Future.successful(TurnDeallocatedResponse)
          }
        case TurnTrySubsume(receiver, lockedNewParent) =>
          localTurnReceiverInstance(receiver) match {
            case Some(turn) =>
              turn.remoteTrySubsume(lookUpLocalLockParameterInstanceWithReference(lockedNewParent, endpoint)).map {
                case Successful  => TurnSuccessfulResponse
                case Blocked     => TurnBlockedResponse
                case Deallocated => TurnDeallocatedResponse
              }(FullMVUtil.notWorthToMoveToTaskpool)
            case None =>
              if (ReactiveTransmittable.DEBUG)
                println(
                  s"[${Thread.currentThread().getName}] $host receiver of request $request was deallocated, dropping remote parameter reference on $lockedNewParent"
                )
              doAsync(endpoint, LockAsyncRemoteRefDropped(lockedNewParent))
              Future.successful(TurnDeallocatedResponse)
          }
        case NewPredecessors(predecessors, clock) =>
          localTurnReflectionReceiverInstance(predecessors.txn).get.newPredecessors(
            predecessors.map { lookUpLocalTurnParameterInstance(_, endpoint) },
            clock
          )

        case LockGetLockedRoot(receiver) =>
          localLockReceiverInstance(receiver) match {
            case Some(lock) => lock.getLockedRoot.map {
                case LockedState(guid)      => LockStateLockedResponse(guid)
                case UnlockedState          => LockStateUnlockedResponse
                case ConcurrentDeallocation => LockStateContendedResponse
              }(FullMVUtil.notWorthToMoveToTaskpool)
            case None =>
              if (ReactiveTransmittable.DEBUG)
                println(s"[${Thread.currentThread().getName}] $host receiver of request $request was deallocated.")
              Future.successful(LockStateContendedResponse)
          }
        case LockTryLock(receiver) =>
          localLockReceiverInstance(receiver) match {
            case Some(lock) =>
              lock.remoteTryLock().map {
                case RemoteLocked(newParent) =>
                  assert(newParent.host == host.lockHost, s"$newParent is not on ${host.lockHost}?!")
                  LockLockedResponse(newParent.guid)
                case RemoteBlocked(newParent) =>
                  assert(newParent.host == host.lockHost, s"$newParent is not on ${host.lockHost}?!")
                  LockBlockedResponse(newParent.guid)
                case RemoteGCd => LockDeallocatedResponse
              }(FullMVUtil.notWorthToMoveToTaskpool)
            case None =>
              if (ReactiveTransmittable.DEBUG)
                println(s"[${Thread.currentThread().getName}] $host receiver of request $request was deallocated")
              Future.successful(LockDeallocatedResponse)
          }
        case LockTrySubsume(receiver, lockedNewParent) =>
          localLockReceiverInstance(receiver) match {
            case Some(lock) =>
              lock.remoteTrySubsume(lookUpLocalLockParameterInstanceWithReference(lockedNewParent, endpoint)).map {
                case RemoteSubsumed => LockSuccessfulResponse
                case RemoteBlocked(newParent) =>
                  assert(newParent.host == host.lockHost, s"$newParent is not on ${host.lockHost}?!")
                  LockBlockedResponse(newParent.guid)
                case RemoteGCd => LockDeallocatedResponse
              }(FullMVUtil.notWorthToMoveToTaskpool)
            case None =>
              if (ReactiveTransmittable.DEBUG)
                println(
                  s"[${Thread.currentThread().getName}] $host receiver of request $request was deallocated, dropping remote parameter reference on $lockedNewParent"
                )
              doAsync(endpoint, LockAsyncRemoteRefDropped(lockedNewParent))
              Future.successful(LockDeallocatedResponse)
          }
      }

    class FullMVTurnMirrorProxyToEndpoint(val guid: Host.GUID, endpoint: EndPointWithInfrastructure[Msg])
        extends FullMVTurnProxy {
      override def addRemoteBranch(forPhase: TurnPhase.Type): Future[Unit] = {
        doRequest(endpoint, AddRemoteBranch(guid, forPhase))
      }

      override def asyncRemoteBranchComplete(forPhase: TurnPhase.Type): Unit = {
        doAsync(endpoint, AsyncRemoteBranchComplete(guid, forPhase))
      }

      override def acquireRemoteBranchIfPhaseAtMost(maxPhase: Type): Future[TurnPhase.Type] = {
        doRequest(endpoint, AcquireRemoteBranchIfAtMost(guid, maxPhase)).map {
          case AcquireRemoteBranchResponse(phase) => phase
        }(FullMVUtil.notWorthToMoveToTaskpool)
      }

      override def addPredecessor(tree: TransactionSpanningTreeNode[FullMVTurn]): Future[Boolean] = {
        doRequest(endpoint, AddPredecessor(guid, tree.map(bundle))).map {
          case BooleanResponse(bool) => bool
        }(FullMVUtil.notWorthToMoveToTaskpool)
      }
      override def maybeNewReachableSubtree(
          attachBelow: FullMVTurn,
          spanningSubTreeRoot: TransactionSpanningTreeNode[FullMVTurn]
      ): Future[Unit] = {
        assert(attachBelow.host == host, s"$attachBelow is not on $host?!")
        doRequest(
          endpoint,
          MaybeNewReachableSubtree(guid, (attachBelow.guid, attachBelow.phase), spanningSubTreeRoot.map(bundle))
        )
      }
      override def newSuccessor(successor: FullMVTurn): Future[Unit] = {
        assert(successor.host == host, s"$successor is not on $host?!")
        doRequest(endpoint, NewSuccessor(guid, (successor.guid, successor.phase)))
      }

      override def getLockedRoot: Future[LockStateResult] = {
        doRequest(endpoint, TurnGetLockedRoot(guid)).map {
          case LockStateLockedResponse(lock)  => LockedState(lock)
          case LockStateUnlockedResponse      => UnlockedState
          case LockStateTurnCompletedResponse => CompletedState
        }(FullMVUtil.notWorthToMoveToTaskpool)
      }
      override def remoteTryLock(): Future[TryLockResult] = {
        doRequest(endpoint, TurnTryLock(guid)).map {
          case TurnLockedResponse(lock) => Locked(lookUpLocalLockParameterInstanceWithReference(lock, endpoint))
          case TurnBlockedResponse      => Blocked
          case TurnDeallocatedResponse  => Deallocated
        }(FullMVUtil.notWorthToMoveToTaskpool)
      }
      override def remoteTrySubsume(lockedNewParent: SubsumableLock): Future[TrySubsumeResult] = {
        assert(lockedNewParent.host == host.lockHost, s"$lockedNewParent is not on ${host.lockHost}?!")
        doRequest(endpoint, TurnTrySubsume(guid, lockedNewParent.guid)).map {
          case TurnSuccessfulResponse  => Successful
          case TurnBlockedResponse     => Blocked
          case TurnDeallocatedResponse => Deallocated
        }(FullMVUtil.notWorthToMoveToTaskpool)
      }

      override def asyncAddPhaseReplicator(
          replicator: FullMVTurnPhaseReflectionProxy,
          knownPhase: TurnPhase.Type
      ): Unit = {
        // safety assumptions for ignoring the replicator parameter here
        assert(
          replicator.isInstanceOf[FullMVTurnReflection],
          s"if this doesn't hold anymore, replicators need to be registered and looked-up in dedicated id->host maps"
        )
        assert(
          host.getInstance(guid) match {
            case Some(instance) =>
              (instance: AnyRef) eq (replicator: AnyRef)
            case None => replicator.asInstanceOf[FullMVTurnReflection].phase == TurnPhase.Completed
          },
          s"replicator should eq turn instance and should be hosted or completed"
        )
        doAsync(endpoint, AsyncAddPhaseReplicator(guid, knownPhase))
      }

      override def asyncAddPredecessorReplicator(
          replicator: FullMVTurnPredecessorReflectionProxy,
          startAt: TransactionSpanningTreeNode[FullMVTurn],
          clock: Type
      ): Unit = {
        // safety assumptions for ignoring the replicator parameter here
        assert(
          replicator.isInstanceOf[FullMVTurnReflection],
          s"if this doesn't hold anymore, replicators need to be registered and looked-up in dedicated id->host maps"
        )
        assert(
          host.getInstance(guid) match {
            case Some(instance) => (instance: AnyRef) eq (replicator: AnyRef)
            case None           => replicator.asInstanceOf[FullMVTurnReflection].phase == TurnPhase.Completed
          },
          s"replicator should eq turn instance and should be hosted or completed"
        )
        doAsync(endpoint, AsyncAddPredecessorReplicator(guid, startAt.map(bundle), clock))
      }
    }

    class FullMVTurnPhaseReflectionProxyToEndpoint(
        val mirroredTurn: FullMVTurn,
        val endpoint: EndPointWithInfrastructure[Msg]
    ) extends FullMVTurnPhaseReflectionProxy {
      override def asyncNewPhase(phase: TurnPhase.Type): Unit = {
        doAsync(endpoint, AsyncNewPhase(mirroredTurn.guid -> mirroredTurn.phase))
      }
    }
    class FullMVTurnPredecessorReflectionProxyToEndpoint(
        val mirroredTurn: FullMVTurn,
        val endpoint: EndPointWithInfrastructure[Msg]
    ) extends FullMVTurnPredecessorReflectionProxy {
      override def newPredecessors(predecessors: TransactionSpanningTreeNode[FullMVTurn], clock: Int): Future[Unit] = {
        doRequest(endpoint, NewPredecessors(predecessors.map(bundle), clock))
      }
    }

    class SubsumableLockProxyToEndpoint(val guid: Host.GUID, endpoint: EndPointWithInfrastructure[Msg])
        extends SubsumableLockProxy {
      override def remoteUnlock(): Unit = {
        doAsync(endpoint, AsyncLockUnlock(guid))
      }
      override def getLockedRoot: Future[LockStateResult0] = {
        doRequest(endpoint, LockGetLockedRoot(guid)).map {
          case LockStateLockedResponse(lock) => LockedState(lock)
          case LockStateUnlockedResponse     => UnlockedState
          case LockStateContendedResponse    => ConcurrentDeallocation
        }(FullMVUtil.notWorthToMoveToTaskpool)
      }
      override def remoteTryLock(): Future[RemoteTryLockResult] = {
        doRequest(endpoint, LockTryLock(guid)).map {
          case LockLockedResponse(lock)  => RemoteLocked(lookUpLocalLockParameterInstanceWithReference(lock, endpoint))
          case LockBlockedResponse(lock) => RemoteBlocked(lookUpLocalLockParameterInstanceWithReference(lock, endpoint))
          case LockDeallocatedResponse   => RemoteGCd
        }(FullMVUtil.notWorthToMoveToTaskpool)
      }
      override def remoteTrySubsume(lockedNewParent: SubsumableLock): Future[RemoteTrySubsumeResult] = {
        assert(lockedNewParent.host == host.lockHost, s"$lockedNewParent is not on ${host.lockHost}?!")
        doRequest(endpoint, LockTrySubsume(guid, lockedNewParent.guid)).map {
          case LockSuccessfulResponse => RemoteSubsumed
          case LockBlockedResponse(newParent) =>
            RemoteBlocked(lookUpLocalLockParameterInstanceWithReference(newParent, endpoint))
          case LockDeallocatedResponse => RemoteGCd
        }(FullMVUtil.notWorthToMoveToTaskpool)
      }
      override def asyncRemoteRefDropped(): Unit = {
        doAsync(endpoint, LockAsyncRemoteRefDropped(guid))
      }
    }

    class ReactiveReflectionProxyToEndpoint(endpoint: EndPointWithInfrastructure[Msg])
        extends ReactiveReflectionProxy[Pulse[P]] {
      override def asyncIncrementFrame(turn: FullMVTurn): Unit =
        doAsync(endpoint, AsyncIncrementFrame(topLevelBundle(turn)))
      override def asyncIncrementSupersedeFrame(turn: FullMVTurn, supersede: FullMVTurn): Unit =
        doAsync(endpoint, AsyncIncrementSupersedeFrame(topLevelBundle(turn), topLevelBundle(supersede)))
      override def asyncResolvedUnchanged(turn: FullMVTurn): Unit =
        doAsync(endpoint, AsyncResolveUnchanged(topLevelBundle(turn)))
      override def asyncResolvedUnchangedFollowFrame(turn: FullMVTurn, followFrame: FullMVTurn): Unit =
        doAsync(endpoint, AsyncResolveUnchangedFollowFrame(topLevelBundle(turn), topLevelBundle(followFrame)))
      override def asyncNewValue(turn: FullMVTurn, value: Pulse[P]): Unit =
        doAsync(endpoint, AsyncNewValue(topLevelBundle(turn), Pluse.fromPulse(value)))
      override def asyncNewValueFollowFrame(turn: FullMVTurn, value: Pulse[P], followFrame: FullMVTurn): Unit =
        doAsync(
          endpoint,
          AsyncNewValueFollowFrame(topLevelBundle(turn), Pluse.fromPulse(value), topLevelBundle(followFrame))
        )
    }
  }

  class DistributedSignal[T, I](persistency: Pulse[T])(implicit
      host: FullMVEngine
  ) extends DistributedReactive[T, Signal, I] {
    override def instantiate(
        state: FullMVState[Pulse[T], FullMVTurn],
        initTurn: FullMVTurn,
        name: String
    ): ReactiveReflectionImpl[Pulse[T]] with Signal[T] =
      new ReactiveReflectionImpl[Pulse[T]](host, None, state, s"SignalReflection($name)")
        with Signal[T] {
        override def disconnect(): Unit = ???
      }
    override val valuePersistency                                         = persistency
    override val ignitionRequiresReevaluation                             = true
    override val isTransient                                              = false
    override def toPulse(reactive: Signal[T]): reactive.Value => Pulse[T] = v => v
  }

  class DistributedEvent[T, I](persistency: Pulse[T])(implicit
      host: FullMVEngine
  ) extends DistributedReactive[T, Event, I] {
    override def instantiate(
        state: FullMVState[Pulse[T], FullMVTurn],
        initTurn: FullMVTurn,
        name: String
    ): ReactiveReflectionImpl[Pulse[T]] with Event[T] =
      new ReactiveReflectionImpl[Pulse[T]](host, None, state, s"SignalReflection($name)")
        with Event[T] {
        override def internalAccess(v: Pulse[T]): Pulse[T] = v
        override def disconnect(): Unit                    = ???
      }
    override val valuePersistency                                        = persistency
    override val ignitionRequiresReevaluation                            = false
    override val isTransient                                             = true
    override def toPulse(reactive: Event[T]): reactive.Value => Pulse[T] = reactive.internalAccess
  }
}
