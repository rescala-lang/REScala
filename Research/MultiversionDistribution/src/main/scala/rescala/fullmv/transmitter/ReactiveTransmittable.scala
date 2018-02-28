package rescala.fullmv.transmitter

import java.io.{ByteArrayInputStream, ByteArrayOutputStream, ObjectInputStream, ObjectOutputStream}
import java.util.concurrent.{ConcurrentHashMap, Executors, ThreadLocalRandom}

import rescala.core.Reactive
import rescala.core._
import rescala.fullmv.TurnPhase.Type
import rescala.fullmv.mirrors._
import rescala.fullmv.sgt.synchronization._
import rescala.fullmv._
import rescala.fullmv.transmitter.ReactiveTransmittable.TurnPushBundle
import rescala.reactives.{Event, Signal}
import retier.transmitter._

import scala.annotation.tailrec
import scala.concurrent._
import scala.util.{Failure, Success}

object ReactiveTransmittable {
  val DEBUG: Boolean = FullMVEngine.DEBUG || SubsumableLock.DEBUG

  type EndPointWithInfrastructure[T] = Endpoint[MessageWithInfrastructure[T], MessageWithInfrastructure[T]]
  type MessageWithInfrastructure[T] = (Long, T)

  type Msg[+T] = (String, Host.GUID, TurnPhase.Type, Host.GUID, TurnPhase.Type, Seq[(Host.GUID, TurnPhase.Type, T)], CaseClassTransactionSpanningTreeNode[(Host.GUID, TurnPhase.Type)], Array[Byte])
  def allEmpty[T](name: String): Msg[T] = (name, Host.dummyGuid, TurnPhase.Uninitialized, Host.dummyGuid, TurnPhase.Uninitialized, Seq.empty, CaseClassTransactionSpanningTreeNode((Host.dummyGuid, TurnPhase.Uninitialized), Array.empty), Array.empty)
  val ASYNC_REQUEST = 0

  sealed trait Message[+P] {
    def toTuple: Msg[P] = this match {
      case UnitResponse => allEmpty("UnitResponse")
      case Connect(turn) => allEmpty("Connect").copy(_2 = turn._1, _3 = turn._2)
      case Initialize(initValues, maybeFirstFrame) => allEmpty("Initialize").copy(_2 = maybeFirstFrame.map(_._1).getOrElse(Host.dummyGuid), _3 = maybeFirstFrame.map(_._2).getOrElse(TurnPhase.Uninitialized), _6 = initValues.map{case ((t,p),v) => (t,p,v)})
      case AsyncIncrementFrame(turn) => allEmpty("AsyncIncrementFrame").copy(_2 = turn._1, _3 = turn._2)
      case AsyncDecrementFrame(turn) => allEmpty("AsyncDecrementFrame").copy(_2 = turn._1, _3 = turn._2)
      case AsyncIncrementSupersedeFrame(turn, supersede) => allEmpty("AsyncIncrementSupersedeFrame").copy(_2 = turn._1, _3 = turn._2, _4 = supersede._1, _5 = supersede._2)
      case AsyncDeframeReframe(turn, reframe) => allEmpty("AsyncDeframeReframe").copy(_2 = turn._1, _3 = turn._2, _4 = reframe._1, _5 = reframe._2)
      case AsyncResolveUnchanged(turn) => allEmpty("AsyncResolveUnchanged").copy(_2 = turn._1, _3 = turn._2)
      case AsyncResolveUnchangedFollowFrame(turn, followFrame) => allEmpty("AsyncResolveUnchangedFollowFrame").copy(_2 = turn._1, _3 = turn._2, _4 = followFrame._1, _5 = followFrame._2)
      case AsyncNewValue(turn, value) => allEmpty("AsyncNewValue").copy(_6 = Seq((turn._1, turn._2, value)))
      case AsyncNewValueFollowFrame(turn, value, followFrame) => allEmpty("AsyncNewValueFollowFrame").copy(_6 = Seq((turn._1, turn._2, value)), _4 = followFrame._1, _5 = followFrame._2)
      case AsyncAddPhaseReplicator(turn, knownPhase) => allEmpty("AsyncAddPhaseReplicator").copy(_2 = turn, _3 = knownPhase)
      case AsyncNewPhase(turn) => allEmpty("AsyncNewPhase").copy(_2 = turn._1, _3 = turn._2)
      case AddPredecessorReplicator(turn) => allEmpty("AddPredecessorReplicator").copy(_2 = turn)
      case PredecessorsResponse(preds) => allEmpty("PredecessorsResponse").copy(_7 = preds)
      case NewPredecessors(newPredecessors) => allEmpty("NewPredecessors").copy(_7 = newPredecessors)
      case AddRemoteBranch(turn, forPhase) => allEmpty("AddRemoteBranch").copy(_2 = turn, _3 = forPhase)
      case AsyncRemoteBranchComplete(turn, forPhase) => allEmpty("AsyncRemoteBranchComplete").copy(_2 = turn, _3 = forPhase)
      case AcquirePhaseLockIfAtMost(turn, phase) => allEmpty("AcquirePhaseLockIfAtMost").copy(_2 = turn, _3 = phase)
      case AcquirePhaseLockResponse(phase) => allEmpty("AcquirePhaseLockResponse").copy(_3 = phase)
      case AddPredecessor(turn, predecessorTree) => allEmpty("AddPredecessor").copy(_2 = turn, _7 = predecessorTree)
      case BooleanResponse(bool) => allEmpty(if(bool) "BooleanTrueResponse" else "BooleanFalseResponse")
      case AsyncReleasePhaseLock(turn) => allEmpty("AsyncReleasePhaseLock").copy(_2 = turn)
      case MaybeNewReachableSubtree(turn, attachBelow, spanningTree) => allEmpty("MaybeNewReachableSubtree").copy(_2 = turn, _4 = attachBelow._1, _5 = attachBelow._2, _7 = spanningTree)
      case NewSuccessor(turn, successor) => allEmpty("NewSuccessor").copy(_2 = turn, _4 = successor._1, _5 = successor._2)
      case TurnGetLockedRoot(turn) => allEmpty("TurnGetLockedRoot").copy(_2 = turn)
      case LockStateTurnCompletedResponse => allEmpty("LockStateTurnCompletedResponse")
      case LockStateLockedResponse(guid) => allEmpty("LockStateLockedResponse").copy(_2 = guid)
      case LockStateUnlockedResponse => allEmpty("LockStateUnlockedResponse")
      case LockStateContendedResponse => allEmpty("LockStateContendedResponse")
      case TurnTryLock(turn) => allEmpty("TurnTryLock").copy(_2 = turn)
      case TurnLockedResponse(lock) => allEmpty("TurnLockedResponse").copy(_2 = lock)
      case TurnTrySubsume(turn, lockedNewParent) => allEmpty("TurnTrySubsume").copy(_2 = turn, _4 = lockedNewParent)
      case TurnBlockedResponse => allEmpty("TurnBlockedResponse")
      case TurnSuccessfulResponse => allEmpty("TurnSuccessfulResponse")
      case TurnDeallocatedResponse => allEmpty("TurnDeallocatedResponse")
      case LockGetLockedRoot(lock) => allEmpty("LockGetLockedRoot").copy(_2 = lock)
      case LockTryLock(lock) => allEmpty("LockTryLock").copy(_2 = lock)
      case LockSuccessfulResponse => allEmpty("LockSuccessfulResponse")
      case LockLockedResponse(newParent) => allEmpty("LockLockedResponse").copy(_2 = newParent)
      case LockTrySubsume(lock, lockedNewParent) => allEmpty("LockTrySubsume").copy(_2 = lock, _4 = lockedNewParent)
      case LockBlockedResponse(lock) => allEmpty("LockBlockedResponse").copy(_2 = lock)
      case LockDeallocatedResponse => allEmpty("LockDeallocatedResponse")
      case LockAsyncUnlock(lock) => allEmpty("LockAsyncUnlock").copy(_2 = lock)
      case LockAsyncRemoteRefDropped(lock) => allEmpty("LockAsyncRemoteRefDropped").copy(_2 = lock)
      case RemoteExceptionResponse(se) => allEmpty("RemoteExceptionResponse").copy(_8 = se)
    }
  }
  object Message {
    def fromTuple[P](tuple: Msg[P]): Message[P] = tuple match {
      case ("UnitResponse", _, _, _, _, _, _, _) => UnitResponse
      case ("Connect", turn, phase, _, _, _, _, _) => Connect(turn -> phase)
      case ("Initialize", maybeFirstFrame, maybeFirstPhase, _, _, initValues, _, firstFrame) => Initialize(initValues.map { case (t,p,v) => ((t, p), v) }, if(maybeFirstFrame != Host.dummyGuid || maybeFirstPhase != TurnPhase.Uninitialized) Some(maybeFirstFrame -> maybeFirstPhase) else None)
      case ("AsyncIncrementFrame", turn, phase, _, _, _, _, _) => AsyncIncrementFrame(turn -> phase)
      case ("AsyncDecrementFrame", turn, phase, _, _, _, _, _) => AsyncDecrementFrame(turn -> phase)
      case ("AsyncIncrementSupersedeFrame", turn, phase, supersede, supersedePhase, _, _, _) => AsyncIncrementSupersedeFrame(turn -> phase, supersede -> supersedePhase)
      case ("AsyncDeframeReframe", turn, phase, reframe, reframePhase, _, _, _) => AsyncDeframeReframe(turn -> phase, reframe -> reframePhase)
      case ("AsyncResolveUnchanged", turn, phase, _, _, _, _, _) => AsyncResolveUnchanged(turn -> phase)
      case ("AsyncResolveUnchangedFollowFrame", turn, phase, followFrame, followPhase, _, _, _) => AsyncResolveUnchangedFollowFrame(turn -> phase, followFrame -> followPhase)
      case ("AsyncNewValue", _, _, _, _, Seq((turn, phase, value)), _, _) => AsyncNewValue(turn -> phase, value)
      case ("AsyncNewValueFollowFrame", _, _, followFrame, followPhase, Seq((turn, phase, value)), _, _) => AsyncNewValueFollowFrame(turn -> phase, value, followFrame -> followPhase)
      case ("AsyncAddPhaseReplicator", turn, knownPhase, _, _, _, _, _) => AsyncAddPhaseReplicator(turn, knownPhase)
      case ("AsyncNewPhase", turn, phase, _, _, _, _, _) => AsyncNewPhase(turn -> phase)
      case ("AddPredecessorReplicator", turn, _, _, _, _, _, _) => AddPredecessorReplicator(turn)
      case ("PredecessorsResponse", _, _, _, _, _, preds, _) => PredecessorsResponse(preds)
      case ("NewPredecessors", _, _, _, _, _, preds, _) => NewPredecessors(preds)
      case ("AddRemoteBranch", turn, phase, _, _, _, _, _) => AddRemoteBranch(turn, phase) // TODO this should not be an assert, but a push phase?
      case ("AsyncRemoteBranchComplete", turn, phase, _, _, _, _, _) => AsyncRemoteBranchComplete(turn, phase)
      case ("AcquirePhaseLockIfAtMost", turn, phase, _, _, _, _, _) => AcquirePhaseLockIfAtMost(turn, phase)
      case ("AcquirePhaseLockResponse", _, phase, _, _, _, _, _) => AcquirePhaseLockResponse(phase)
      case ("AddPredecessor", turn, _, _, _, _, tree, _) => AddPredecessor(turn, tree)
      case ("BooleanTrueResponse", _, _, _, _, _, _, _) => BooleanResponse(true)
      case ("BooleanFalseResponse", _, _, _, _, _, _, _) => BooleanResponse(false)
      case ("AsyncReleasePhaseLock", turn, _, _, _, _, _, _) => AsyncReleasePhaseLock(turn)
      case ("MaybeNewReachableSubtree", turn, _, attachBelow, attachPhase, _, tree, _) => MaybeNewReachableSubtree(turn, attachBelow -> attachPhase, tree)
      case ("NewSuccessor", turn, _, successor, successorPhase, _, _, _) => NewSuccessor(turn, successor -> successorPhase)
      case ("TurnGetLockedRoot", turn, _, _, _, _, _, _) => TurnGetLockedRoot(turn)
      case ("LockStateTurnCompletedResponse", _, _, _, _, _, _, _) => LockStateTurnCompletedResponse
      case ("LockStateLockedResponse", guid, _, _, _, _, _, _) => LockStateLockedResponse(guid)
      case ("LockStateUnlockedResponse", _, _, _, _, _, _, _) => LockStateUnlockedResponse
      case ("LockStateContendedResponse", _, _, _, _, _, _, _) => LockStateContendedResponse
      case ("TurnTryLock", turn, _, _, _, _, _, _) => TurnTryLock(turn)
      case ("TurnLockedResponse", lock, _, _, _, _, _, _) => TurnLockedResponse(lock)
      case ("TurnTrySubsume", turn, _, lock, _, _, _, _) => TurnTrySubsume(turn, lock)
      case ("TurnBlockedResponse", _, _, _, _, _, _, _) => TurnBlockedResponse
      case ("TurnSuccessfulResponse", _, _, _, _, _, _, _) => TurnSuccessfulResponse
      case ("TurnDeallocatedResponse", _, _, _, _, _, _, _) => TurnDeallocatedResponse
      case ("LockGetLockedRoot", lock, _, _, _, _, _, _) => LockGetLockedRoot(lock)
      case ("LockTryLock", lock, _, _, _, _, _, _) => LockTryLock(lock)
      case ("LockSuccessfulResponse", _, _, _, _, _, _, _) => LockSuccessfulResponse
      case ("LockLockedResponse", newParent, _, _, _, _, _, _) => LockLockedResponse(newParent)
      case ("LockTrySubsume", lock, _, newParent, _, _, _, _) => LockTrySubsume(lock, newParent)
      case ("LockBlockedResponse", newParent, _, _, _, _, _, _) => LockBlockedResponse(newParent)
      case ("LockDeallocatedResponse", _, _, _, _, _, _, _) => LockDeallocatedResponse
      case ("LockAsyncUnlock", lock, _, _, _, _, _, _) => LockAsyncUnlock(lock)
      case ("LockAsyncRemoteRefDropped", lock, _, _, _, _, _, _) => LockAsyncRemoteRefDropped(lock)
      case ("RemoteExceptionResponse", _, _, _, _, _, _, se) => RemoteExceptionResponse(se)
      case otherwise =>
        val e = new AssertionError("Unrecognized message: " + otherwise)
        e.printStackTrace()
        throw e
    }
  }
  sealed trait PossiblyBlockingTopLevel[+P] extends Message[P]
  sealed trait UnderlyingChatter extends Message[Nothing]

  sealed trait Async[+P] extends Message[P]
  sealed trait PossiblyBlockingTopLevelAsync[+P] extends Async[P] with PossiblyBlockingTopLevel[P]
  sealed trait UnderlyingChatterAsync extends Async[Nothing] with UnderlyingChatter

  sealed trait Request[+P] extends Message[P] {
    type Response <: ReactiveTransmittable.this.Response[P]
  }
  sealed trait PossiblyBlockingTopLevelRequest[+P] extends Request[P] with PossiblyBlockingTopLevel[P]
  sealed trait UnderlyingChatterRequest extends Request[Nothing] with UnderlyingChatter

  sealed trait Response[+P] extends Message[P]
  case class RemoteExceptionResponse(serializedThrowable: Array[Byte]) extends Response[Nothing]
  case object UnitResponse extends Response[Nothing]
  import scala.language.implicitConversions
  implicit def unitResponseToUnitFuture(future: Future[UnitResponse.type]): Future[Unit] = future.map(_ => ())(FullMVEngine.notWorthToMoveToTaskpool)
  implicit def unitToUnitResponseFuture(future: Future[Unit]): Future[UnitResponse.type] = future.map(_ => UnitResponse)(FullMVEngine.notWorthToMoveToTaskpool)

  type TurnPushBundle = (Host.GUID, TurnPhase.Type)

  /** Connection Establishment **/
  case class Connect[P](turn: TurnPushBundle) extends PossiblyBlockingTopLevelRequest[P]{ override type Response = Initialize[P] }
  case class Initialize[P](initValues: Seq[(TurnPushBundle, P)], maybeFirstFrame: Option[TurnPushBundle]) extends Response[P]
  /** [[ReactiveReflectionProxy]] **/
  case class AsyncIncrementFrame(turn: TurnPushBundle) extends PossiblyBlockingTopLevelAsync[Nothing]
  case class AsyncDecrementFrame(turn: TurnPushBundle) extends PossiblyBlockingTopLevelAsync[Nothing]
  case class AsyncIncrementSupersedeFrame(turn: TurnPushBundle, supersede: TurnPushBundle) extends PossiblyBlockingTopLevelAsync[Nothing]
  case class AsyncDeframeReframe(turn: TurnPushBundle, reframe: TurnPushBundle) extends PossiblyBlockingTopLevelAsync[Nothing]
  case class AsyncResolveUnchanged(turn: TurnPushBundle) extends PossiblyBlockingTopLevelAsync[Nothing]
  case class AsyncResolveUnchangedFollowFrame(turn: TurnPushBundle, followFrame: TurnPushBundle) extends PossiblyBlockingTopLevelAsync[Nothing]
  case class AsyncNewValue[P](turn: TurnPushBundle, value: P) extends PossiblyBlockingTopLevelAsync[P]
  case class AsyncNewValueFollowFrame[P](turn: TurnPushBundle, value: P, followFrame: TurnPushBundle) extends PossiblyBlockingTopLevelAsync[P]
  /** [[FullMVTurnProxy]] **/
  case class AsyncAddPhaseReplicator(turn: Host.GUID, alreadyKnownPhase: TurnPhase.Type) extends UnderlyingChatterAsync
  case class AddPredecessorReplicator(turn: Host.GUID) extends UnderlyingChatterRequest { override type Response = PredecessorsResponse }
  case class PredecessorsResponse(predecessors: CaseClassTransactionSpanningTreeNode[TurnPushBundle]) extends Response[Nothing]
  case class AddRemoteBranch(turn: Host.GUID, forPhase: TurnPhase.Type) extends UnderlyingChatterRequest { override type Response = UnitResponse.type }
  case class AsyncRemoteBranchComplete(turn: Host.GUID, forPhase: TurnPhase.Type) extends UnderlyingChatterAsync
  case class AcquirePhaseLockIfAtMost(turn: Host.GUID, phase: TurnPhase.Type) extends UnderlyingChatterRequest{ override type Response = AcquirePhaseLockResponse }
  case class AcquirePhaseLockResponse(phase: TurnPhase.Type) extends Response[Nothing]
  case class AddPredecessor(turn: Host.GUID, predecessorTree: CaseClassTransactionSpanningTreeNode[TurnPushBundle]) extends UnderlyingChatterRequest{ override type Response = BooleanResponse }
  case class BooleanResponse(bool: Boolean) extends Response[Nothing]
  case class AsyncReleasePhaseLock(turn: Host.GUID) extends UnderlyingChatterAsync
  case class MaybeNewReachableSubtree(turn: Host.GUID, attachBelow: TurnPushBundle, spanningTree: CaseClassTransactionSpanningTreeNode[TurnPushBundle]) extends UnderlyingChatterRequest{ override type Response = UnitResponse.type }
  case class NewSuccessor(turn: Host.GUID, successor: TurnPushBundle) extends UnderlyingChatterRequest{ override type Response = UnitResponse.type }
  /** [[SubsumableLockEntryPoint]] **/
  case class TurnGetLockedRoot(turn: Host.GUID) extends UnderlyingChatterRequest{ override type Response = TurnLockStateResponse }
  sealed trait TurnLockStateResponse extends Response[Nothing]
  case object LockStateTurnCompletedResponse extends TurnLockStateResponse
  case class TurnTryLock(turn: Host.GUID) extends UnderlyingChatterRequest{ override type Response = TurnTryLockResponse }
  sealed trait TurnTryLockResponse extends Response[Nothing]
  case class TurnLockedResponse(lock: Host.GUID) extends TurnTryLockResponse
  case class TurnTrySubsume(turn: Host.GUID, lockedNewParent: Host.GUID) extends UnderlyingChatterRequest{ override type Response = TurnTrySubsumeResponse }
  sealed trait TurnTrySubsumeResponse extends Response[Nothing]
  case object TurnSuccessfulResponse extends TurnTrySubsumeResponse
  case object TurnBlockedResponse extends TurnTryLockResponse with TurnTrySubsumeResponse
  case object TurnDeallocatedResponse extends TurnTryLockResponse with TurnTrySubsumeResponse
  /** [[SubsumableLockProxy]] **/
  case class LockGetLockedRoot(lock: Host.GUID) extends UnderlyingChatterRequest{ override type Response = LockStateResponse }
  sealed trait LockStateResponse extends Response[Nothing]
  case class LockStateLockedResponse(guid: Host.GUID) extends LockStateResponse with TurnLockStateResponse
  case object LockStateUnlockedResponse extends LockStateResponse with TurnLockStateResponse
  case object LockStateContendedResponse extends LockStateResponse
  case class LockTryLock(lock: Host.GUID) extends UnderlyingChatterRequest{ override type Response = LockTryLockResponse }
  sealed trait LockTryLockResponse extends Response[Nothing]
  case class LockLockedResponse(lock: Host.GUID) extends LockTryLockResponse
  case class LockTrySubsume(lock: Host.GUID, lockedNewParent: Host.GUID) extends UnderlyingChatterRequest{ override type Response = LockTrySubsumeResponse }
  sealed trait LockTrySubsumeResponse extends Response[Nothing]
  case object LockSuccessfulResponse extends LockTrySubsumeResponse
  case class LockBlockedResponse(lock: Host.GUID) extends LockTryLockResponse with LockTrySubsumeResponse
  case object LockDeallocatedResponse extends LockTryLockResponse with LockTrySubsumeResponse
  case class LockAsyncUnlock(lock: Host.GUID) extends UnderlyingChatterAsync
  case class LockAsyncRemoteRefDropped(lock: Host.GUID) extends UnderlyingChatterAsync
  /** [[FullMVTurnPhaseReflectionProxy]] **/
  case class AsyncNewPhase(turn: TurnPushBundle) extends UnderlyingChatterAsync
  /** [[FullMVTurnPredecessorReflectionProxy]] **/
  case class NewPredecessors(newPredecessors: CaseClassTransactionSpanningTreeNode[TurnPushBundle]) extends UnderlyingChatterRequest { override type Response = UnitResponse.type }

  def deserializeThrowable(se: Array[Byte]): Throwable = {
    val bais = new ByteArrayInputStream(se)
    val ois = new ObjectInputStream(bais)
    ois.readObject().asInstanceOf[Throwable]
  }
  def serializeThrowable(t: Throwable): Array[Byte] = {
    val baos = new ByteArrayOutputStream()
    val oos = new ObjectOutputStream(baos)
    oos.writeObject(t)
    oos.flush()
    baos.toByteArray
  }
  sealed trait Pluse[+P] {
    def toPulse: Pulse[P] = this match {
      case Pluse.NoChange => Pulse.NoChange
      case Pluse.Value(v) => Pulse.Value(v)
      case Pluse.Exceptional(se) => Pulse.Exceptional(deserializeThrowable(se))
    }
  }
  object Pluse {
    def fromPulse[P](pulse: Pulse[P]): Pluse[P] = {
      pulse match {
        case Pulse.NoChange => Pluse.NoChange
        case Pulse.Value(v) => Pluse.Value(v)
        case Pulse.Exceptional(t) => Pluse.Exceptional(serializeThrowable(t))
      }
    }
    case object NoChange extends Pluse[Nothing]
    final case class Value[+P](update: P) extends Pluse[P]
    final case class Exceptional(serializedThrowable: Array[Byte]) extends Pluse[Nothing]
  }

  implicit def signalTransmittable[P, S](implicit
                                         host: FullMVEngine,
                                         messageTransmittable: Transmittable[MessageWithInfrastructure[Msg[Pluse[P]]], S, MessageWithInfrastructure[Msg[Pluse[P]]]],
                                         serializable: Serializable[S]
                                        ): Transmittable[Signal[P, FullMVStruct], S, Signal[P, FullMVStruct]] =
    new ReactiveTransmittable[P, Signal[P, FullMVStruct], S] {
    override def instantiate(state: NodeVersionHistory[Pulse[P], FullMVTurn, ReSource[FullMVStruct], Reactive[FullMVStruct]], initTurn: FullMVTurn): ReactiveReflectionImpl[Pulse[P]] with Signal[P, FullMVStruct] =
      new ReactiveReflectionImpl[Pulse[P]](host, None, state, "SignalReflection") with Signal[P, FullMVStruct] {
        override def disconnect()(implicit engine: Scheduler[FullMVStruct]): Unit = ???
      }
    override val valuePersistency: Initializer.InitValues[Pulse[P]] = Initializer.DerivedSignal[P]
    override val ignitionRequiresReevaluation = true
    override val isTransient = false
    override def toPulse(reactive: Signal[P, FullMVStruct]): reactive.Value => Pulse[P] = v => v
  }
  implicit def eventTransmittable[P, S](implicit host: FullMVEngine, messageTransmittable: Transmittable[MessageWithInfrastructure[Msg[Pluse[P]]], S, MessageWithInfrastructure[Msg[Pluse[P]]]], serializable: Serializable[S]): Transmittable[Event[P, FullMVStruct], S, Event[P, FullMVStruct]] = new ReactiveTransmittable[P, Event[P, FullMVStruct], S] {
    override def instantiate(state: NodeVersionHistory[Pulse[P], FullMVTurn, ReSource[FullMVStruct], Reactive[FullMVStruct]], initTurn: FullMVTurn): ReactiveReflectionImpl[Pulse[P]] with Event[P, FullMVStruct] =
      new ReactiveReflectionImpl[Pulse[P]](host, Some(initTurn), state, "EventReflection") with Event[P, FullMVStruct] {
        override def internalAccess(v: Pulse[P]): Pulse[P] = v
        override def disconnect()(implicit engine: Scheduler[FullMVStruct]): Unit = ???
      }
    override val valuePersistency: Initializer.InitValues[Pulse[P]] = Initializer.Event[P]
    override val ignitionRequiresReevaluation = false
    override val isTransient = true
    override def toPulse(reactive: Event[P, FullMVStruct]): reactive.Value => Pulse[P] = reactive.internalAccess
  }
}

abstract class ReactiveTransmittable[P, R <: ReSource[FullMVStruct], S](implicit val host: FullMVEngine, messageTransmittable: Transmittable[ReactiveTransmittable.MessageWithInfrastructure[ReactiveTransmittable.Msg[ReactiveTransmittable.Pluse[P]]], S, ReactiveTransmittable.MessageWithInfrastructure[ReactiveTransmittable.Msg[ReactiveTransmittable.Pluse[P]]]], serializable: Serializable[S]) extends PushBasedTransmittable[R, ReactiveTransmittable.MessageWithInfrastructure[ReactiveTransmittable.Msg[ReactiveTransmittable.Pluse[P]]], S, ReactiveTransmittable.MessageWithInfrastructure[ReactiveTransmittable.Msg[ReactiveTransmittable.Pluse[P]]], R] {
  def bundle(turn: FullMVTurn): TurnPushBundle = {
    assert(turn.host == host, s"$turn is not on $host?!")
    (turn.guid, turn.phase)
  }

  import ReactiveTransmittable._
  type Msg = ReactiveTransmittable.Msg[Pluse[P]]
  type Message = ReactiveTransmittable.Message[Pluse[P]]
  type Response = ReactiveTransmittable.Response[Pluse[P]]

  val executeInTaskPool: ExecutionContext = ExecutionContext.fromExecutor(Executors.newCachedThreadPool())
  val requestTracker = new ConcurrentHashMap[Long, Promise[_ <: Response]]()

  def handleChatter(endpoint: EndPointWithInfrastructure[Msg], requestId: Long, message: UnderlyingChatter): Unit = {
    message match {
      case async: UnderlyingChatterAsync =>
        executeInTaskPool.execute(new Runnable {
          override def run(): Unit = {
            if(ReactiveTransmittable.DEBUG) println(s"[${Thread.currentThread().getName}] $host processing async $async")
            try {
              handleAsyncChatter(endpoint, async)
            } catch {
              // TODO cannot propagate this back to sender because async, what else to do?
              case t: Throwable => new Exception("Error processing async " + async, t).printStackTrace()
            }
          }
        })
      case request: UnderlyingChatterRequest =>
        executeInTaskPool.execute(new Runnable {
          override def run(): Unit = {
            if(ReactiveTransmittable.DEBUG) println(s"[${Thread.currentThread().getName}] $host processing request $request")
            try {
              handleRequestChatter(endpoint, request).onComplete {
                case Success(response) =>
                  if(ReactiveTransmittable.DEBUG) println(s"[${Thread.currentThread().getName}] $host replying $requestId: $response")
                  endpoint.send((requestId, response.toTuple))
                case Failure(t) =>
                  if(ReactiveTransmittable.DEBUG) println(s"[${Thread.currentThread().getName}] $host request $requestId completed, but resulted in failure; returning ${t.getClass.getName}: ${t.getMessage} to remote")
                  if(ReactiveTransmittable.DEBUG) t.printStackTrace()
                  endpoint.send(requestId -> RemoteExceptionResponse(serializeThrowable(t)).toTuple)
              }(FullMVEngine.notWorthToMoveToTaskpool)
            } catch {
              case t: Throwable =>
                if(ReactiveTransmittable.DEBUG) println(s"[${Thread.currentThread().getName}] $host request $requestId failed to execute; returning ${t.getClass.getName}: ${t.getMessage} to remote")
                if(ReactiveTransmittable.DEBUG) t.printStackTrace()
                endpoint.send(requestId -> RemoteExceptionResponse(serializeThrowable(t)).toTuple)
            }
          }
        })
    }
  }

  def consumeResponse(requestId: Long, response: Response): Unit = {
    try {
      val promise = requestTracker.remove(requestId).asInstanceOf[Promise[Response]] /* typesafety yay */
      assert(promise != null, s"request $requestId unknown!")
      promise.complete(response match {
        case RemoteExceptionResponse(se) => Failure(deserializeThrowable(se))
        case otherwise => Success(response)
      })
    } catch {
      // this should only happen on framework bugs. Since we're just completing a future, everything client-code that happens as
      // a reaction of that should catch and propagate exceptions itself, so no client exceptions should be thrown uncaught..
      case t: Throwable => t.printStackTrace()
    }
  }

  override def send(reactive: R, remote: RemoteRef, endpoint: EndPointWithInfrastructure[Msg]): MessageWithInfrastructure[Msg] = {
    if(ReactiveTransmittable.DEBUG) println(s"[${Thread.currentThread().getName}] $host sending $reactive")
    endpoint.receive.notify { mwi: MessageWithInfrastructure[Msg] =>
      if(ReactiveTransmittable.DEBUG) println(s"[${Thread.currentThread().getName}] sender $host receive incoming $mwi")
      val (requestId: Long, msg: Msg) = mwi
      Message.fromTuple(msg) match {
        case response: Response =>
          consumeResponse(requestId, response)
        case Connect(turnBundle) =>
          host.threadPool.submit(new Runnable {
            override def run(): Unit = {
              try {
                if (ReactiveTransmittable.DEBUG) println(s"[${Thread.currentThread().getName}] $host processing connect $reactive $turnBundle")
                val (initValues, maybeFirstFrame) = ReactiveMirror[Pulse[P]](reactive, lookUpLocalTurnParameterInstance(turnBundle, endpoint), isTransient, s"Mirror($endpoint)")(toPulse(reactive), new ReactiveReflectionProxyToEndpoint(endpoint))
                val response = Initialize(initValues.map { case (aTurn, value) =>
                  (bundle(aTurn), Pluse.fromPulse(value))
                }, maybeFirstFrame.map(bundle))
                endpoint.send(requestId -> response.toTuple)
              } catch {
                case t: Throwable =>
                  if(ReactiveTransmittable.DEBUG) println(s"[${Thread.currentThread().getName}] $host connect $reactive $turnBundle failed to execute; returning ${t.getClass.getName}: ${t.getMessage} to remote")
                  if(ReactiveTransmittable.DEBUG) t.printStackTrace()
                  endpoint.send(requestId -> RemoteExceptionResponse(serializeThrowable(t)).toTuple)
              }
            }
          })
        case otherwise: PossiblyBlockingTopLevel[P] => new Exception("Illegal top level message received on sender side: " + otherwise)
        case otherwise: UnderlyingChatter => handleChatter(endpoint, requestId, otherwise)
      }
    }
    (0L, UnitResponse.toTuple)
  }

  def doAsync(endpoint: EndPointWithInfrastructure[Msg], parameters: Async[Pluse[P]]): Unit = {
    if(ReactiveTransmittable.DEBUG) println(s"[${Thread.currentThread().getName}] $host send async $parameters")
    endpoint.send((0L, parameters.toTuple))
  }

  def doRequest(endpoint: EndPointWithInfrastructure[Msg],  parameters: Request[Pluse[P]]): Future[parameters.Response] = {
    val promise = Promise[parameters.Response]()
    @inline @tailrec def createRequest(): Long = {
      val requestId = ThreadLocalRandom.current().nextLong()
      if(requestId != 0 && requestTracker.putIfAbsent(requestId, promise) == null) requestId else createRequest()
    }
    val requestId = createRequest()
    if(ReactiveTransmittable.DEBUG) println(s"[${Thread.currentThread().getName}] $host send request $requestId: $parameters")
    endpoint.send((requestId, parameters.toTuple))
    promise.future
  }

  def lookUpLocalLockParameterInstanceWithReference(guid: Host.GUID, endpoint: EndPointWithInfrastructure[Msg]): SubsumableLock = {
    host.lockHost.getCachedOrReceiveRemoteWithReference(guid, new SubsumableLockProxyToEndpoint(guid, endpoint))
  }

  def localLockReceiverInstance(guid: Host.GUID): Option[SubsumableLockProxy] = {
    val instance = host.lockHost.getInstance(guid)
    if(ReactiveTransmittable.DEBUG) {
      if(instance.isDefined) {
        println(s"[${Thread.currentThread().getName}] $host retrieved chached receiver $instance")
      } else {
        println(s"[${Thread.currentThread().getName}] $host receiver lock lookup failed for $guid, should have been concurrently gc'd.")
      }
    }
    instance
  }

  def getKnownLocalTurnParameterInstance(bundle: TurnPushBundle, endpoint: EndPointWithInfrastructure[Msg]): FullMVTurn = {
    val turn = host.getInstance(bundle._1).get
    if(turn.isInstanceOf[FullMVTurnReflection]) turn.asInstanceOf[FullMVTurnReflection].asyncNewPhase(bundle._2)
    turn
  }

  def lookUpLocalTurnParameterInstance(bundle: TurnPushBundle, endpoint: EndPointWithInfrastructure[Msg]): FullMVTurn = {
    val (guid, phase) = bundle
    val active = phase < TurnPhase.Completed
    if(active) {
      host.getCachedOrReceiveRemote(guid, new FullMVTurnReflection(host, guid, phase, new FullMVTurnMirrorProxyToEndpoint(guid, endpoint))) match {
        case Instantiated(reflection) =>
          reflection.asyncNewPhase(phase)
          doAsync(endpoint, AsyncAddPhaseReplicator(guid, phase))
          reflection
        case Found(reflection: FullMVTurnReflection) =>
          reflection.asyncNewPhase(phase)
          reflection
        case Found(notReflection) =>
          assert(phase <= notReflection.phase, s"apparently $notReflection has a newer phase ($phase) on a remote copy than the local original?")
          notReflection
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

  val valuePersistency: Initializer.InitValues[Pulse[P]]

  val ignitionRequiresReevaluation: Boolean
  val isTransient: Boolean
  def toPulse(reactive: R): reactive.Value => Pulse[P]

  override def receive(value: MessageWithInfrastructure[Msg], remote: RemoteRef, endpoint: EndPointWithInfrastructure[Msg]): R = {
    if(ReactiveTransmittable.DEBUG) println(s"[${Thread.currentThread().getName}] $host receiving a value")
    val turn = host.newTurn()
    turn.beginExecuting()
    val state = turn.makeDerivedStructState[Pulse[P]](valuePersistency)
    val reflection = instantiate(state, turn)
    if(ReactiveTransmittable.DEBUG) println(s"[${Thread.currentThread().getName}] $host instantiating $reflection, requesting initialization starting at $turn")
    endpoint.receive.notify { mwi: MessageWithInfrastructure[Msg] =>
      if(ReactiveTransmittable.DEBUG) println(s"[${Thread.currentThread().getName}] receiver $host receive incoming $mwi")
      val (requestId: Long, msg: Msg) = mwi
      Message.fromTuple(msg) match {
        case response: Response =>
          consumeResponse(requestId, response)
        case topLevel: PossiblyBlockingTopLevelAsync[Pluse[P]] =>
          host.threadPool.submit(new Runnable {
            override def run(): Unit = {
              if (ReactiveTransmittable.DEBUG) println(s"[${Thread.currentThread().getName}] $host processing top level async $topLevel")
              try {
                handleTopLevelAsync(reflection, endpoint, topLevel)
              } catch {
                // TODO cannot propagate this back to sender because async, what else to do?
                case t: Throwable => new Exception("Error processing top level async " + topLevel, t).printStackTrace()
              }
            }
          })
        case otherwise: PossiblyBlockingTopLevel[P] => new Exception("Illegal top level message received on receiver side: " + otherwise)
        case otherwise: UnderlyingChatter => handleChatter(endpoint, requestId, otherwise)
      }
    }
    doRequest(endpoint, Connect[Pluse[P]](bundle(turn))).foreach {
      case Initialize(initValues, maybeFirstFrame) =>
        if(ReactiveTransmittable.DEBUG) println(s"[${Thread.currentThread().getName}] $host received initialization package for $reflection")
        val reflectionInitValues = initValues.map { case (mirrorTurn, v) =>
          val turn = lookUpLocalTurnParameterInstance(mirrorTurn, endpoint)
          turn.ensurePredecessorReplication()
          turn -> v
        }
        val reflectionMaybeFirstFrame = maybeFirstFrame.map { mirrorTurn =>
          val turn = lookUpLocalTurnParameterInstance(mirrorTurn, endpoint)
          turn.ensurePredecessorReplication()
          turn
        }

        state.retrofitSinkFrames(reflectionInitValues.map(_._1), reflectionMaybeFirstFrame, +1)
        for((reflectionTurn, v) <- reflectionInitValues) reflection.buffer(reflectionTurn, v.toPulse)

        turn.ignite(reflection, Set.empty, ignitionRequiresReevaluation)

        turn.completeExecuting()
    }(executeInTaskPool)

    reflection
  }

  def instantiate(state: NodeVersionHistory[Pulse[P], FullMVTurn, ReSource[FullMVStruct], Reactive[FullMVStruct]], initTurn: FullMVTurn): ReactiveReflectionImpl[Pulse[P]] with R

  def handleTopLevelAsync(reflection: ReactiveReflection[Pulse[P]], endpoint: EndPointWithInfrastructure[Msg], message: PossiblyBlockingTopLevelAsync[Pluse[P]]): Unit = message match {
    case AsyncIncrementFrame(turn) =>
      reflection.asyncIncrementFrame(lookUpLocalTurnParameterInstance(turn, endpoint))
    case AsyncDecrementFrame(turn) =>
      reflection.asyncDecrementFrame(lookUpLocalTurnParameterInstance(turn, endpoint))
    case AsyncIncrementSupersedeFrame(turn, supersede) =>
      reflection.asyncIncrementSupersedeFrame(lookUpLocalTurnParameterInstance(turn, endpoint), lookUpLocalTurnParameterInstance(supersede, endpoint))
    case AsyncDeframeReframe(turn, reframe) =>
      reflection.asyncDeframeReframe(lookUpLocalTurnParameterInstance(turn, endpoint), lookUpLocalTurnParameterInstance(reframe, endpoint))
    case AsyncResolveUnchanged(turn) =>
      reflection.asyncResolvedUnchanged(lookUpLocalTurnParameterInstance(turn, endpoint))
    case AsyncResolveUnchangedFollowFrame(turn, followFrame) =>
      reflection.asyncResolvedUnchangedFollowFrame(lookUpLocalTurnParameterInstance(turn, endpoint), lookUpLocalTurnParameterInstance(followFrame, endpoint))
    case AsyncNewValue(turn, value) =>
      reflection.asyncNewValue(lookUpLocalTurnParameterInstance(turn, endpoint), value.toPulse)
    case AsyncNewValueFollowFrame(turn, value, followFrame) =>
      reflection.asyncNewValueFollowFrame(lookUpLocalTurnParameterInstance(turn, endpoint), value.toPulse, lookUpLocalTurnParameterInstance(followFrame, endpoint))
  }

  def handleAsyncChatter(endpoint: EndPointWithInfrastructure[Msg], message: UnderlyingChatterAsync): Unit = message match {
    case AsyncRemoteBranchComplete(receiver, forPhase) =>
      val maybeTurn = localTurnReceiverInstance(receiver)
      assert(maybeTurn.isDefined, s"supposedly a remote still has a branch, but $maybeTurn has already been deallocated")
      maybeTurn.get.asyncRemoteBranchComplete(forPhase)
    case AsyncReleasePhaseLock(receiver) =>
      val maybeTurn = localTurnReceiverInstance(receiver)
      assert(maybeTurn.isDefined, s"$maybeTurn was deallocated while a remote supposedly held its phase lock")
      maybeTurn.get.asyncReleasePhaseLock()

    case LockAsyncUnlock(receiver) =>
      val lock = localLockReceiverInstance(receiver)
      assert(lock.isDefined, s"unlock should only be called along paths on which a reference is held, so concurrent deallocation should be impossible.")
      lock.get.remoteAsyncUnlock()
    case LockAsyncRemoteRefDropped(receiver) =>
      val lock = localLockReceiverInstance(receiver)
      assert(lock.isDefined, s"a reference should only be dropped if it currently is held, so concurrent deallocation should be impossible.")
      lock.get.asyncRemoteRefDropped()

    case AsyncAddPhaseReplicator(receiver, known) =>
      localTurnReceiverInstance(receiver) match {
        case Some(turn) => turn.asyncAddPhaseReplicator(new FullMVTurnPhaseReflectionProxyToEndpoint(turn, endpoint), known)
        case None =>
          if(ReactiveTransmittable.DEBUG) println(s"[${Thread.currentThread().getName}] $host receiver of request $message was deallocated; ignoring call as it is no longer needed")
          doAsync(endpoint, AsyncNewPhase(receiver -> TurnPhase.Completed))
      }
    case AsyncNewPhase(bundle) =>
      localTurnReflectionReceiverInstance(bundle)
  }

  private def sendTree(predTree: TransactionSpanningTreeNode[FullMVTurn]) = {
    predTree.map { predPred =>
      assert(predPred.host == host, s"$predPred is not on $host?!")
      (predPred.guid, predPred.phase)
    }
  }

  def handleRequestChatter(endpoint: EndPointWithInfrastructure[Msg], request: UnderlyingChatterRequest): Future[Response] = request match {
    case AddRemoteBranch(receiver, forPhase) =>
      val maybeTurn = localTurnReceiverInstance(receiver)
      assert(maybeTurn.isDefined, s"someone tried to revive $receiver, which should thus not have been possible to be deallocated")
      maybeTurn.get.addRemoteBranch(forPhase)
    case AcquirePhaseLockIfAtMost(receiver, phase) =>
      localTurnReceiverInstance(receiver) match {
        case Some(turn) => turn.acquirePhaseLockIfAtMost(phase).map(AcquirePhaseLockResponse)(FullMVEngine.notWorthToMoveToTaskpool)
        case None =>
          if(ReactiveTransmittable.DEBUG) println(s"[${Thread.currentThread().getName}] $host receiver of request $request was deallocated")
          Future.successful(AcquirePhaseLockResponse(TurnPhase.Completed))
      }
    case MaybeNewReachableSubtree(receiver, attachBelow, spanningSubTreeRoot) =>
      val maybeTurn = localTurnReceiverInstance(receiver)
      assert(maybeTurn.isDefined, s"someone tried to share possible transitive predecessors with $receiver, which should thus not have been possible to be deallocated")
      maybeTurn.get.maybeNewReachableSubtree(getKnownLocalTurnParameterInstance(attachBelow, endpoint), spanningSubTreeRoot.map { lookUpLocalTurnParameterInstance(_, endpoint) })
    case AddPredecessor(receiver, predecessorTree) =>
      val maybeTurn = localTurnReceiverInstance(receiver)
      assert(maybeTurn.isDefined, s"someone tried to add predecessors on turn $receiver, which should thus not have been possible to be deallocated")
      maybeTurn.get.addPredecessor(predecessorTree.map { lookUpLocalTurnParameterInstance(_, endpoint) }).map(BooleanResponse)(FullMVEngine.notWorthToMoveToTaskpool)
    case NewSuccessor(receiver, successor) =>
      localTurnReceiverInstance(receiver) match {
        case Some(turn) => turn.newSuccessor(lookUpLocalTurnParameterInstance(successor, endpoint))
        case None =>
          if(ReactiveTransmittable.DEBUG) println(s"[${Thread.currentThread().getName}] $host receiver of request $request was deallocated; ignoring call as it is no longer needed")
          Future.successful(UnitResponse)
      }
    case TurnGetLockedRoot(receiver) =>
      localTurnReceiverInstance(receiver) match {
        case Some(turn) => turn.getLockedRoot.map {
          case LockedState(guid) => LockStateLockedResponse(guid)
          case UnlockedState => LockStateUnlockedResponse
          case CompletedState => LockStateTurnCompletedResponse
        }(FullMVEngine.notWorthToMoveToTaskpool)
        case None =>
          if(ReactiveTransmittable.DEBUG) println(s"[${Thread.currentThread().getName}] $host receiver of request $request was deallocated")
          Future.successful(LockStateTurnCompletedResponse)
      }
    case TurnTryLock(receiver) =>
      localTurnReceiverInstance(receiver) match {
        case Some(turn) => turn.remoteTryLock().map {
          case Locked(lock) =>
            assert(lock.host == host.lockHost, s"$lock is not on ${host.lockHost}?!")
            TurnLockedResponse(lock.guid)
          case Blocked => TurnBlockedResponse
          case Deallocated => TurnDeallocatedResponse
        }(FullMVEngine.notWorthToMoveToTaskpool)
        case None =>
          if(ReactiveTransmittable.DEBUG) println(s"[${Thread.currentThread().getName}] $host receiver of request $request was deallocated")
          Future.successful(TurnDeallocatedResponse)
      }
    case TurnTrySubsume(receiver, lockedNewParent) =>
      localTurnReceiverInstance(receiver) match {
        case Some(turn) => turn.remoteTrySubsume(lookUpLocalLockParameterInstanceWithReference(lockedNewParent, endpoint)).map {
          case Successful => TurnSuccessfulResponse
          case Blocked => TurnBlockedResponse
          case Deallocated => TurnDeallocatedResponse
        }(FullMVEngine.notWorthToMoveToTaskpool)
        case None =>
          if(ReactiveTransmittable.DEBUG) println(s"[${Thread.currentThread().getName}] $host receiver of request $request was deallocated, dropping remote parameter reference on $lockedNewParent")
          doAsync(endpoint, LockAsyncRemoteRefDropped(lockedNewParent))
          Future.successful(TurnDeallocatedResponse)
      }
    case NewPredecessors(predecessors) =>
      localTurnReflectionReceiverInstance(predecessors.txn).get.newPredecessors(predecessors.map { lookUpLocalTurnParameterInstance(_, endpoint) })


    case LockGetLockedRoot(receiver) =>
      localLockReceiverInstance(receiver) match {
        case Some(lock) => lock.getLockedRoot.map{
          case LockedState(guid) => LockStateLockedResponse(guid)
          case UnlockedState => LockStateUnlockedResponse
          case ConcurrentDeallocation => LockStateContendedResponse
        }(FullMVEngine.notWorthToMoveToTaskpool)
        case None =>
          if(ReactiveTransmittable.DEBUG) println(s"[${Thread.currentThread().getName}] $host receiver of request $request was deallocated.")
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
          }(FullMVEngine.notWorthToMoveToTaskpool)
        case None =>
          if(ReactiveTransmittable.DEBUG) println(s"[${Thread.currentThread().getName}] $host receiver of request $request was deallocated")
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
          }(FullMVEngine.notWorthToMoveToTaskpool)
        case None =>
          if(ReactiveTransmittable.DEBUG) println(s"[${Thread.currentThread().getName}] $host receiver of request $request was deallocated, dropping remote parameter reference on $lockedNewParent")
          doAsync(endpoint, LockAsyncRemoteRefDropped(lockedNewParent))
          Future.successful(LockDeallocatedResponse)
      }
    case AddPredecessorReplicator(receiver) =>
      localTurnReceiverInstance(receiver) match {
        case Some(turn) =>
          turn.addPredecessorReplicator(new FullMVTurnPredecessorReflectionProxyToEndpoint(turn, endpoint)).map{ preds =>
            PredecessorsResponse(preds.map(bundle))
          }(FullMVEngine.notWorthToMoveToTaskpool)
        case None =>
          if(ReactiveTransmittable.DEBUG) println(s"[${Thread.currentThread().getName}] $host receiver of request $request was deallocated")
          Future.successful(PredecessorsResponse(CaseClassTransactionSpanningTreeNode(receiver -> TurnPhase.Completed, Array.empty)))
      }
  }

  class FullMVTurnMirrorProxyToEndpoint(val guid: Host.GUID, endpoint: EndPointWithInfrastructure[Msg]) extends FullMVTurnProxy {
    override def addRemoteBranch(forPhase: TurnPhase.Type): Future[Unit] = {
      doRequest(endpoint, AddRemoteBranch(guid, forPhase))
    }

    override def asyncRemoteBranchComplete(forPhase: TurnPhase.Type): Unit = {
      doAsync(endpoint, AsyncRemoteBranchComplete(guid, forPhase))
    }

    override def acquirePhaseLockIfAtMost(maxPhase: Type): Future[TurnPhase.Type] = {
      doRequest(endpoint, AcquirePhaseLockIfAtMost(guid, maxPhase)).map {
        case AcquirePhaseLockResponse(phase) => phase
      }(executeInTaskPool)
    }

    override def addPredecessor(tree: TransactionSpanningTreeNode[FullMVTurn]): Future[Boolean] = {
      doRequest(endpoint, AddPredecessor(guid, sendTree(tree))).map {
        case BooleanResponse(bool) => bool
      }(FullMVEngine.notWorthToMoveToTaskpool)
    }
    override def asyncReleasePhaseLock(): Unit = {
      doAsync(endpoint, AsyncReleasePhaseLock(guid))
    }
    override def maybeNewReachableSubtree(attachBelow: FullMVTurn, spanningSubTreeRoot: TransactionSpanningTreeNode[FullMVTurn]): Future[Unit] = {
      assert(attachBelow.host == host, s"$attachBelow is not on $host?!")
      doRequest(endpoint, MaybeNewReachableSubtree(guid, (attachBelow.guid, attachBelow.phase), sendTree(spanningSubTreeRoot)))
    }
    override def newSuccessor(successor: FullMVTurn): Future[Unit] = {
      assert(successor.host == host, s"$successor is not on $host?!")
      doRequest(endpoint, NewSuccessor(guid, (successor.guid, successor.phase)))
    }

    override def getLockedRoot: Future[LockStateResult] = {
      doRequest(endpoint, TurnGetLockedRoot(guid)).map {
        case LockStateLockedResponse(lock) => LockedState(lock)
        case LockStateUnlockedResponse => UnlockedState
        case LockStateTurnCompletedResponse => CompletedState
      }(FullMVEngine.notWorthToMoveToTaskpool)
    }
    override def remoteTryLock(): Future[TryLockResult] = {
      doRequest(endpoint, TurnTryLock(guid)).map {
        case TurnLockedResponse(lock) => Locked(lookUpLocalLockParameterInstanceWithReference(lock, endpoint))
        case TurnBlockedResponse => Blocked
        case TurnDeallocatedResponse => Deallocated
      }(executeInTaskPool)
    }
    override def remoteTrySubsume(lockedNewParent: SubsumableLock): Future[TrySubsumeResult] = {
      assert(lockedNewParent.host == host.lockHost, s"$lockedNewParent is not on ${host.lockHost}?!")
      doRequest(endpoint, TurnTrySubsume(guid, lockedNewParent.guid)).map {
        case TurnSuccessfulResponse => Successful
        case TurnBlockedResponse => Blocked
        case TurnDeallocatedResponse => Deallocated
      }(executeInTaskPool)
    }

    override def asyncAddPhaseReplicator(replicator: FullMVTurnPhaseReflectionProxy, knownPhase: TurnPhase.Type): Unit = {
      // safety assumptions for ignoring the replicator parameter here
      assert(replicator.isInstanceOf[FullMVTurnReflection], s"if this doesn't hold anymore, replicators need to be registered and looked-up in dedicated id->host maps")
      assert(host.getInstance(guid) match {
        case Some(instance) => instance eq replicator
        case None => replicator.asInstanceOf[FullMVTurnReflection].phase == TurnPhase.Completed
      }, s"replicator should eq turn instance and should be hosted or completed")
      doAsync(endpoint, AsyncAddPhaseReplicator(guid, knownPhase))
    }
    override def addPredecessorReplicator(replicator: FullMVTurnPredecessorReflectionProxy): Future[TransactionSpanningTreeNode[FullMVTurn]] = {
      // safety assumptions for ignoring the replicator parameter here
      assert(replicator.isInstanceOf[FullMVTurnReflection], s"if this doesn't hold anymore, replicators need to be registered and looked-up in dedicated id->host maps")
      assert(host.getInstance(guid) match {
        case Some(instance) => instance eq replicator
        case None => replicator.asInstanceOf[FullMVTurnReflection].phase == TurnPhase.Completed
      }, s"replicator should eq turn instance and should be hosted or completed")
      doRequest(endpoint, AddPredecessorReplicator(guid)).map {
        case PredecessorsResponse(predecessors) => predecessors.map { lookUpLocalTurnParameterInstance(_, endpoint) }
      }(FullMVEngine.notWorthToMoveToTaskpool)
    }
  }

  class FullMVTurnPhaseReflectionProxyToEndpoint(val mirroredTurn: FullMVTurn, endpoint: EndPointWithInfrastructure[Msg]) extends FullMVTurnPhaseReflectionProxy {
    override def asyncNewPhase(phase: TurnPhase.Type): Unit = {
      doAsync(endpoint, AsyncNewPhase(mirroredTurn.guid -> mirroredTurn.phase))
    }
  }
  class FullMVTurnPredecessorReflectionProxyToEndpoint(val mirroredTurn: FullMVTurn, endpoint: EndPointWithInfrastructure[Msg]) extends FullMVTurnPredecessorReflectionProxy {
    override def newPredecessors(predecessors: TransactionSpanningTreeNode[FullMVTurn]): Future[Unit] = {
      doRequest(endpoint, NewPredecessors(sendTree(predecessors)))
    }
  }

  class SubsumableLockProxyToEndpoint(val guid: Host.GUID, endpoint: EndPointWithInfrastructure[Msg]) extends SubsumableLockProxy {
    override def remoteAsyncUnlock(): Unit = {
      doAsync(endpoint, LockAsyncUnlock(guid))
    }
    override def getLockedRoot: Future[LockStateResult0] = {
      doRequest(endpoint, LockGetLockedRoot(guid)).map {
        case LockStateLockedResponse(lock) => LockedState(lock)
        case LockStateUnlockedResponse => UnlockedState
        case LockStateContendedResponse => ConcurrentDeallocation
      }(FullMVEngine.notWorthToMoveToTaskpool)
    }
    override def remoteTryLock(): Future[RemoteTryLockResult] = {
      doRequest(endpoint, LockTryLock(guid)).map {
        case LockLockedResponse(lock) => RemoteLocked(lookUpLocalLockParameterInstanceWithReference(lock, endpoint))
        case LockBlockedResponse(lock) => RemoteBlocked(lookUpLocalLockParameterInstanceWithReference(lock, endpoint))
        case LockDeallocatedResponse => RemoteGCd
      }(executeInTaskPool)
    }
    override def remoteTrySubsume(lockedNewParent: SubsumableLock): Future[RemoteTrySubsumeResult] = {
      assert(lockedNewParent.host == host.lockHost, s"$lockedNewParent is not on ${host.lockHost}?!")
      doRequest(endpoint, LockTrySubsume(guid, lockedNewParent.guid)).map {
        case LockSuccessfulResponse => RemoteSubsumed
        case LockBlockedResponse(newParent) => RemoteBlocked(lookUpLocalLockParameterInstanceWithReference(newParent, endpoint))
        case LockDeallocatedResponse => RemoteGCd
      }(executeInTaskPool)
    }
    override def asyncRemoteRefDropped(): Unit = {
      doAsync(endpoint, LockAsyncRemoteRefDropped(guid))
    }
  }

  class ReactiveReflectionProxyToEndpoint(endpoint: EndPointWithInfrastructure[Msg]) extends ReactiveReflectionProxy[Pulse[P]] {
    override def asyncIncrementFrame(turn: FullMVTurn): Unit = doAsync(endpoint, AsyncIncrementFrame(bundle(turn)))
    override def asyncDecrementFrame(turn: FullMVTurn): Unit = doAsync(endpoint, AsyncDecrementFrame(bundle(turn)))
    override def asyncIncrementSupersedeFrame(turn: FullMVTurn, supersede: FullMVTurn): Unit = doAsync(endpoint, AsyncIncrementSupersedeFrame(bundle(turn), bundle(supersede)))
    override def asyncDeframeReframe(turn: FullMVTurn, reframe: FullMVTurn): Unit = doAsync(endpoint, AsyncDeframeReframe(bundle(turn), bundle(reframe)))
    override def asyncResolvedUnchanged(turn: FullMVTurn): Unit = doAsync(endpoint, AsyncResolveUnchanged(bundle(turn)))
    override def asyncResolvedUnchangedFollowFrame(turn: FullMVTurn, followFrame: FullMVTurn): Unit = doAsync(endpoint, AsyncResolveUnchangedFollowFrame(bundle(turn), bundle(followFrame)))
    override def asyncNewValue(turn: FullMVTurn, value: Pulse[P]): Unit = doAsync(endpoint, AsyncNewValue(bundle(turn), Pluse.fromPulse(value)))
    override def asyncNewValueFollowFrame(turn: FullMVTurn, value: Pulse[P], followFrame: FullMVTurn): Unit = doAsync(endpoint, AsyncNewValueFollowFrame(bundle(turn), Pluse.fromPulse(value), bundle(followFrame)))
  }
}
