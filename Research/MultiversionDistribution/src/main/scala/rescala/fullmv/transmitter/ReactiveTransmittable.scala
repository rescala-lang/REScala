package rescala.fullmv.transmitter

import java.io.{ByteArrayInputStream, ByteArrayOutputStream, ObjectInputStream, ObjectOutputStream}
import java.util.concurrent.{ConcurrentHashMap, ThreadLocalRandom}

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

sealed trait MessageType
case object AsyncRequest extends MessageType
case class Request(requestId: Long) extends MessageType
case class Response(requestId: Long) extends MessageType

object ReactiveTransmittable {
  val DEBUG: Boolean = FullMVEngine.DEBUG

  type EndPointWithInfrastructure[T] = Endpoint[MessageWithInfrastructure[T], MessageWithInfrastructure[T]]
  type MessageWithInfrastructure[T] = (Long, T)

//  type Msg[+T] = (String, Host.GUID, Host.GUID, Option[T], Seq[(Host.GUID, T)], TurnPhase.Type, CaseClassTransactionSpanningTreeNode[Host.GUID], Boolean, Long, Seq[Host.GUID])
//  def allEmpty[T](name: String): Msg[T] = (name, Host.dummyGuid, Host.dummyGuid, None, Seq.empty, TurnPhase.dummy, CaseClassTransactionSpanningTreeNode[Host.GUID](Host.dummyGuid, Array.empty), false, 0L, Seq.empty)
  val ASYNC_REQUEST = 0

  sealed trait Message[+P] {
//    def toTuple: Msg[P] = this match {
//      case UnitResponse => allEmpty("UnitResponse")
//      case Connect(turn) => allEmpty("Connect").copy(_2 = turn)
//      case Initialize(initValues, maybeFirstFrame) => allEmpty("Initialize").copy(_2 = maybeFirstFrame.getOrElse(Host.dummyGuid), _5 = initValues, _8 = maybeFirstFrame.isDefined)
//      case AsyncIncrementFrame(turn) => allEmpty("AsyncIncrementFrame").copy(_2 = turn)
//      case AsyncDecrementFrame(turn) => allEmpty("AsyncDecrementFrame").copy(_2 = turn)
//      case AsyncIncrementSupersedeFrame(turn, supersede) => allEmpty("AsyncIncrementSupersedeFrame").copy(_2 = turn, _3 = supersede)
//      case AsyncDeframeReframe(turn, reframe) => allEmpty("AsyncDeframeReframe").copy(_2 = turn, _3 = reframe)
//      case AsyncResolveUnchanged(turn) => allEmpty("AsyncResolveUnchanged").copy(_2 = turn)
//      case AsyncResolveUnchangedFollowFrame(turn, followFrame) => allEmpty("AsyncResolveUnchangedFollowFrame").copy(_2 = turn, _3 = followFrame)
//      case AsyncNewValue(turn, value) => allEmpty("AsyncNewValue").copy(_2 = turn, _4 = Some(value))
//      case AsyncNewValueFollowFrame(turn, value, followFrame) => allEmpty("AsyncNewValueFollowFrame").copy(_2 = turn, _3 = followFrame, _4 = Some(value))
//      case AddReplicator(turn) => allEmpty("AddReplicator").copy(_2 = turn)
//      case AddReplicatorResponse(initPhase, initPreds) => allEmpty("AddReplicatorResponse").copy(_6 = initPhase, _7 = initPreds)
//      case AddRemoteBranch(turn, forPhase) => allEmpty("AddRemoteBranch").copy(_2 = turn, _6 = forPhase)
//      case AsyncRemoteBranchComplete(turn, forPhase) => allEmpty("AsyncRemoteBranchComplete").copy(_2 = turn, _6 = forPhase)
//      case AcquirePhaseLockAndGetEstablishmentBundle(turn) => allEmpty("AcquirePhaseLockAndGetEstablishmentBundle").copy(_2 = turn)
//      case AcquirePhaseLockAndGetEstablishmentBundleResponse(phase, predecessorTree) => allEmpty("AcquirePhaseLockAndGetEstablishmentBundleResponse").copy(_6 = phase, _7 = predecessorTree)
//      case AddPredecessorAndReleasePhaseLock(turn, predecessorTree) => allEmpty("AddPredecessorAndReleasePhaseLock").copy(_2 = turn, _7 = predecessorTree)
//      case AsyncReleasePhaseLock(turn) => allEmpty("AsyncReleasePhaseLock").copy(_2 = turn)
//      case MaybeNewReachableSubtree(turn, attachBelow, spanningTree) => allEmpty("MaybeNewReachableSubtree").copy(_2 = turn, _3 = attachBelow, _7 = spanningTree)
//      case NewSuccessor(turn, successor) => allEmpty("NewSuccessor").copy(_2 = turn, _3 = successor)
//      case TurnGetLockedRoot(turn) => allEmpty("TurnGetLockedRoot").copy(_2 = turn)
//      case MaybeLockResponse(newParentIfFailed) => allEmpty("MaybeLockResponse").copy(_2 = newParentIfFailed.getOrElse(Host.dummyGuid), _8 = newParentIfFailed.isDefined)
//      case TurnLock(turn) => allEmpty("TurnLock").copy(_2 = turn)
//      case TurnTrySubsume(turn, lockedNewParent) => allEmpty("TurnTrySubsume").copy(_2 = turn, _3 = lockedNewParent)
//      case TurnTrySubsumeResponse(Blocked) => allEmpty("BooleanResponse").copy(_9 = 0L)
//      case TurnTrySubsumeResponse(Successful) => allEmpty("BooleanResponse").copy(_9 = 1L)
//      case TurnTrySubsumeResponse(Deallocated) => allEmpty("BooleanResponse").copy(_9 = -1L)
//      case LockGetLockedRoot(lock) => allEmpty("LockGetLockedRoot").copy(_2 = lock)
//      case LockLock(lock) => allEmpty("LockLock").copy(_2 = lock)
//      case LockSpinOnce(lock, backoff) => allEmpty("LockSpinOnce").copy(_2 = lock, _9 = backoff)
//      case LockResponse(newParent) => allEmpty("LockResponse").copy(_2 = newParent)
//      case LockTrySubsume(lock, lockedNewParent) => allEmpty("LockTrySubsume").copy(_2 = lock, _3 = lockedNewParent)
//      case LockUnlock(lock) => allEmpty("LockUnlock").copy(_2 = lock)
//      case LockRemoteRefDropped(lock) => allEmpty("LockRemoteRefDropped").copy(_2 = lock)
//      case NewPredecessors(turn, newPredecessors) => allEmpty("NewPredecessors").copy(_2 = turn, _7 = newPredecessors)
//      case NewPhase(turn, phase) => allEmpty("NewPhase").copy(_2 = turn, _6 = phase)
//    }
  }
//  object Message {
//    def fromTuple[P](tuple: Msg[P]): Message[P] = tuple match {
//      case ("UnitResponse", _, _, _, _, _, _, _, _, _) => UnitResponse
//      case ("Connect", turn, _, _, _, _, _, _, _, _) => Connect(turn)
//      case ("Initialize", mbff, _, _, initValues, _, _, mb, _, _) => Initialize(initValues, if(mb) Some(mbff) else None)
//      case ("AsyncIncrementFrame", turn, _, _, _, _, _, _, _, _) => AsyncIncrementFrame(turn)
//      case ("AsyncDecrementFrame", turn, _, _, _, _, _, _, _, _) => AsyncDecrementFrame(turn)
//      case ("AsyncIncrementSupersedeFrame", turn, supersede, _, _, _, _, _, _, _) => AsyncIncrementSupersedeFrame(turn, supersede)
//      case ("AsyncDeframeReframe", turn, reframe, _, _, _, _, _, _, _) => AsyncDeframeReframe(turn, reframe)
//      case ("AsyncResolveUnchanged", turn, _, _, _, _, _, _, _, _) => AsyncResolveUnchanged(turn)
//      case ("AsyncResolveUnchangedFollowFrame", turn, followFrame, _, _, _, _, _, _, _) => AsyncResolveUnchangedFollowFrame(turn, followFrame)
//      case ("AsyncNewValue", turn, _, Some(value), _, _, _, _, _, _) => AsyncNewValue(turn, value)
//      case ("AsyncNewValueFollowFrame", turn, followFrame, Some(value), _, _, _, _, _, _) => AsyncNewValueFollowFrame(turn, value, followFrame)
//      case ("AddReplicator", turn, _, _, _, _, _, _, _, _) => AddReplicator(turn)
//      case ("AddReplicatorResponse", _, _, _, _, initPhase, initPreds, _, _, _) => AddReplicatorResponse(initPhase, initPreds)
//      case ("AddRemoteBranch", turn, _, _, _, forPhase, _, _, _, _) => AddRemoteBranch(turn, forPhase)
//      case ("AsyncRemoteBranchComplete", turn, _, _, _, forPhase, _, _, _, _)=> AsyncRemoteBranchComplete(turn, forPhase)
//      case ("AcquirePhaseLockAndGetEstablishmentBundle", turn, _, _, _, _, _, _, _, _) => AcquirePhaseLockAndGetEstablishmentBundle(turn)
//      case ("AcquirePhaseLockAndGetEstablishmentBundleResponse", _, _, _, _, phase, predecessorTree, _, _, _) => AcquirePhaseLockAndGetEstablishmentBundleResponse(phase, predecessorTree)
//      case ("AddPredecessorAndReleasePhaseLock", turn, _, _, _, _, predecessorTree, _, _, _) => AddPredecessorAndReleasePhaseLock(turn, predecessorTree)
//      case ("AsyncReleasePhaseLock", turn, _, _, _, _, _, _, _, _) => AsyncReleasePhaseLock(turn)
//      case ("MaybeNewReachableSubtree", turn, attachBelow, _, _, _, spanningTree, _, _, _) => MaybeNewReachableSubtree(turn, attachBelow, spanningTree)
//      case ("NewSuccessor", turn, successor, _, _, _, _, _, _, _) => NewSuccessor(turn, successor)
//      case ("TurnGetLockedRoot", turn, _, _, _, _, _, _, _, _) => TurnGetLockedRoot(turn)
//      case ("MaybeLockResponse", mbnp, _, _, _, _, _, mb, _, _) => MaybeLockResponse(if(mb) Some(mbnp) else None)
//      case ("TurnLock", turn, _, _, _, _, _, _, _, _) => TurnLock(turn)
//      case ("TurnTrySubsume", turn, lockedNewParent, _, _, _, _, _, _, _) => TurnTrySubsume(turn, lockedNewParent)
//      case ("TurnTrySubsumeResponse", _, _, _, _, _, _, _, 0L, _) => TurnTrySubsumeResponse(Blocked)
//      case ("TurnTrySubsumeResponse", _, _, _, _, _, _, _, 1L, _) => TurnTrySubsumeResponse(Successful)
//      case ("TurnTrySubsumeResponse", _, _, _, _, _, _, _, -1L, _) => TurnTrySubsumeResponse(Deallocated)
//      case ("LockGetLockedRoot", lock, _, _, _, _, _, _, _, _) => LockGetLockedRoot(lock)
//      case ("LockLock", lock, _, _, _, _, _, _, _, _) => LockLock(lock)
//      case ("LockSpinOnce", lock, _, _, _, _, _, _, backoff, _) => LockSpinOnce(lock, backoff)
//      case ("LockResponse", newParent, _, _, _, _, _, _, _, _) => LockResponse(newParent)
//      case ("LockTrySubsume", lock, lockedNewParent, _, _, _, _, _, _, _) => LockTrySubsume(lock, lockedNewParent)
//      case ("LockUnlock", lock, _, _, _, _, _, _, _, _) => LockUnlock(lock)
//      case ("LockRemoteRefDropped", lock, _, _, _, _, _, _, _, _) => LockRemoteRefDropped(lock)
//      case ("NewPredecessors", turn, _, _, _, _, newPredecessors, _, _, _) => NewPredecessors(turn, newPredecessors)
//      case ("NewPhase", turn, _, _, _, phase, _, _, _, _) => NewPhase(turn, phase)
//      case otherwise =>
//        val e = new AssertionError("Unrecognized message: " + otherwise)
//        e.printStackTrace()
//        throw e
//    }
//  }
  sealed trait Async[+P] extends Message[P]
  sealed trait Request[+P] extends Message[P] {
    type Response <: ReactiveTransmittable.this.Response[P]
  }
  sealed trait Response[+P] extends Message[P]
  case object UnitResponse extends Response[Nothing]
  type TurnPushBundle = (Host.GUID, TurnPhase.Type, CaseClassTransactionSpanningTreeNode[(Host.GUID, TurnPhase.Type)])

  /** Connection Establishment **/
  case class Connect[P](turn: TurnPushBundle) extends Request[P]{ override type Response = Initialize[P] }
  case class Initialize[P](initValues: Seq[(TurnPushBundle, P)], maybeFirstFrame: Option[TurnPushBundle]) extends Response[P]
  /** [[ReactiveReflectionProxy]] **/
  case class AsyncIncrementFrame(turn: TurnPushBundle) extends Async[Nothing]
  case class AsyncDecrementFrame(turn: TurnPushBundle) extends Async[Nothing]
  case class AsyncIncrementSupersedeFrame(turn: TurnPushBundle, supersede: TurnPushBundle) extends Async[Nothing]
  case class AsyncDeframeReframe(turn: TurnPushBundle, reframe: TurnPushBundle) extends Async[Nothing]
  case class AsyncResolveUnchanged(turn: TurnPushBundle) extends Async[Nothing]
  case class AsyncResolveUnchangedFollowFrame(turn: TurnPushBundle, followFrame: TurnPushBundle) extends Async[Nothing]
  case class AsyncNewValue[P](turn: TurnPushBundle, value: P) extends Async[P]
  case class AsyncNewValueFollowFrame[P](turn: TurnPushBundle, value: P, followFrame: TurnPushBundle) extends Async[P]
  /** [[FullMVTurnProxy]] **/
  case class AsyncAddReplicator(turns: Set[Host.GUID]) extends Async[Nothing]
  case class AsyncAddReplicatorSyncData(syncedData: Map[Host.GUID, (TurnPhase.Type, CaseClassTransactionSpanningTreeNode[(Host.GUID, TurnPhase.Type)])]) extends Async[Nothing]
  case class AddRemoteBranch(turn: Host.GUID, forPhase: TurnPhase.Type) extends Request[Nothing] { override type Response = UnitResponse.type }
  case class AsyncRemoteBranchComplete(turn: Host.GUID, forPhase: TurnPhase.Type) extends Async[Nothing]
  case class AcquirePhaseLockIfAtMost(turn: Host.GUID, phase: TurnPhase.Type) extends Request[Nothing]{ override type Response = AcquirePhaseLockResponse }
  case class AcquirePhaseLockResponse(phase: TurnPhase.Type) extends Response[Nothing]
  case class AddPredecessor(turn: Host.GUID, predecessorTree: CaseClassTransactionSpanningTreeNode[(Host.GUID, TurnPhase.Type)]) extends Request[Nothing]{ override type Response = UnitResponse.type }
  case class AsyncReleasePhaseLock(turn: Host.GUID) extends Async[Nothing]
  case class MaybeNewReachableSubtree(turn: Host.GUID, attachBelow: Host.GUID, spanningTree: CaseClassTransactionSpanningTreeNode[(Host.GUID, TurnPhase.Type)]) extends Request[Nothing]{ override type Response = UnitResponse.type }
  case class NewSuccessor(turn: Host.GUID, successor: Host.GUID) extends Request[Nothing]{ override type Response = UnitResponse.type }
  /** [[SubsumableLockEntryPoint]] && [[SubsumableLockProxy]] **/
  case class MaybeLockResponse(newParentIfFailed: Option[Host.GUID]) extends Response[Nothing]
  /** [[SubsumableLockEntryPoint]] **/
  case class TurnGetLockedRoot(turn: Host.GUID) extends Request[Nothing]{ override type Response = MaybeLockResponse }
  case class TurnTryLock(turn: Host.GUID) extends Request[Nothing]{ override type Response = TurnTryLockResponse }
  sealed trait TurnTryLockResponse extends Response[Nothing]
  case class TurnLockedResponse(lock: Host.GUID) extends TurnTryLockResponse
  case class TurnTrySubsume(turn: Host.GUID, lockedNewParent: Host.GUID) extends Request[Nothing]{ override type Response = TurnTrySubsumeResponse }
  sealed trait TurnTrySubsumeResponse extends Response[Nothing]
  case object TurnSuccessfulResponse extends TurnTrySubsumeResponse
  case object TurnBlockedResponse extends TurnTryLockResponse with TurnTrySubsumeResponse
  case object TurnDeallocatedResponse extends TurnTryLockResponse with TurnTrySubsumeResponse
  /** [[SubsumableLockProxy]] **/
  case class LockGetLockedRoot(lock: Host.GUID) extends Request[Nothing]{ override type Response = MaybeLockResponse }
  case class LockTryLock(lock: Host.GUID) extends Request[Nothing]{ override type Response = LockTryLockResponse }
  sealed trait LockTryLockResponse extends Response[Nothing]
  case class LockLockedResponse(lock: Host.GUID) extends LockTryLockResponse
  case class LockTrySubsume(lock: Host.GUID, lockedNewParent: Host.GUID) extends Request[Nothing]{ override type Response = LockTrySubsumeResponse }
  sealed trait LockTrySubsumeResponse extends Response[Nothing]
  case object LockSuccessfulResponse extends LockTrySubsumeResponse
  case class LockBlockedResponse(lock: Host.GUID) extends LockTryLockResponse with LockTrySubsumeResponse
  case object LockDeallocatedResponse extends LockTryLockResponse with LockTrySubsumeResponse
  case class LockAsyncUnlock(lock: Host.GUID) extends Async[Nothing]
  case class LockAsyncRemoteRefDropped(lock: Host.GUID) extends Async[Nothing]
  /** [[FullMVTurnReflectionProxy]] **/
  case class NewPredecessors(turn: Host.GUID, newPredecessors: CaseClassTransactionSpanningTreeNode[(Host.GUID, TurnPhase.Type)]) extends Request[Nothing]{ override type Response = UnitResponse.type }
  case class NewPhase(turn: Host.GUID, phase: TurnPhase.Type) extends Request[Nothing]{ override type Response = UnitResponse.type }

  sealed trait Pluse[+P] {
    def toPulse: Pulse[P] = this match {
      case Pluse.NoChange => Pulse.NoChange
      case Pluse.Value(v) => Pulse.Value(v)
      case Pluse.Exceptional(se) =>
        val bais = new ByteArrayInputStream(se)
        val ois = new ObjectInputStream(bais)
        val e = ois.readObject().asInstanceOf[Throwable]
        Pulse.Exceptional(e)
    }
  }
  object Pluse {
    def fromPulse[P](pulse: Pulse[P]): Pluse[P] = {
      pulse match {
        case Pulse.NoChange => Pluse.NoChange
        case Pulse.Value(v) => Pluse.Value(v)
        case Pulse.Exceptional(e) =>
          val baos = new ByteArrayOutputStream()
          val oos = new ObjectOutputStream(baos)
          oos.writeObject(e)
          oos.flush()
          Pluse.Exceptional(baos.toByteArray)
      }
    }
    case object NoChange extends Pluse[Nothing]
    final case class Value[+P](update: P) extends Pluse[P]
    final case class Exceptional(serializedThrowable: Array[Byte]) extends Pluse[Nothing]
  }

  implicit def signalTransmittable[P, S](implicit host: FullMVEngine, messageTransmittable: Transmittable[MessageWithInfrastructure[Message[Pluse[P]]], S, MessageWithInfrastructure[Message[Pluse[P]]]], serializable: Serializable[S]): Transmittable[Signal[P, FullMVStruct], S, Signal[P, FullMVStruct]] = new ReactiveTransmittable[P, Signal[P, FullMVStruct], S] {
    override def instantiate(state: NodeVersionHistory[Pulse[P], FullMVTurn, ReSource[FullMVStruct], Reactive[FullMVStruct]], initTurn: FullMVTurn) =
      new ReactiveReflectionImpl[P](host, None, state, "SignalReflection") with Signal[P, FullMVStruct] {
        override def disconnect()(implicit engine: Scheduler[FullMVStruct]): Unit = ???
      }
    override val valuePersistency: Initializer.Param[Pulse[P]] = Initializer.DerivedSignal[P]
  }
  implicit def eventTransmittable[P, S](implicit host: FullMVEngine, messageTransmittable: Transmittable[MessageWithInfrastructure[Message[Pluse[P]]], S, MessageWithInfrastructure[Message[Pluse[P]]]], serializable: Serializable[S]): Transmittable[Event[P, FullMVStruct], S, Event[P, FullMVStruct]] = new ReactiveTransmittable[P, Event[P, FullMVStruct], S] {
    override def instantiate(state: NodeVersionHistory[Pulse[P], FullMVTurn, ReSource[FullMVStruct], Reactive[FullMVStruct]], initTurn: FullMVTurn) =
      new ReactiveReflectionImpl[P](host, Some(initTurn), state, "EventReflection") with Event[P, FullMVStruct] {
        override def disconnect()(implicit engine: Scheduler[FullMVStruct]): Unit = ???
      }
    override val valuePersistency: Initializer.Param[Pulse[P]] = Initializer.Event[P]
  }
}

abstract class ReactiveTransmittable[P, R <: ReSourciV[Pulse[P], FullMVStruct], S](implicit val host: FullMVEngine, messageTransmittable: Transmittable[ReactiveTransmittable.MessageWithInfrastructure[ReactiveTransmittable.Message[ReactiveTransmittable.Pluse[P]]], S, ReactiveTransmittable.MessageWithInfrastructure[ReactiveTransmittable.Message[ReactiveTransmittable.Pluse[P]]]], serializable: Serializable[S]) extends PushBasedTransmittable[R, ReactiveTransmittable.MessageWithInfrastructure[ReactiveTransmittable.Message[ReactiveTransmittable.Pluse[P]]], S, ReactiveTransmittable.MessageWithInfrastructure[ReactiveTransmittable.Message[ReactiveTransmittable.Pluse[P]]], R] {
  def bundle(turn: FullMVTurn): TurnPushBundle = {
    assert(turn.host == host)
    (turn.guid, turn.phase, sendTree(turn.selfNode))
  }

  import ReactiveTransmittable._
  type Msg = ReactiveTransmittable.Message[Pluse[P]]
  type Message = ReactiveTransmittable.Message[Pluse[P]]
  type Async = ReactiveTransmittable.Async[Pluse[P]]
  type Request = ReactiveTransmittable.Request[Pluse[P]]
  type Response = ReactiveTransmittable.Response[Pluse[P]]

  val executeInTaskPool: ExecutionContext = ExecutionContext.fromExecutorService(host.threadPool)

  val requestTracker = new ConcurrentHashMap[Long, Promise[_ <: Response]]()

  def handleResponse(requestId: Long, response: Response): Unit = {
    val promise = requestTracker.remove(requestId).asInstanceOf[Promise[Response]] /* typesafety yay */
    assert(promise != null, s"request $requestId unknown!")
    promise.complete(Success(response))
  }

  def handleMessage(localReactive: Either[ReSourciV[Pulse[P], FullMVStruct], ReactiveReflection[P]], endpoint: EndPointWithInfrastructure[Msg])(msg: MessageWithInfrastructure[Msg]): Unit = {
    if(ReactiveTransmittable.DEBUG) println(s"[${Thread.currentThread().getName}] $host receive incoming $msg")
    (msg._1, /*Message.fromTuple*/(msg._2)) match {
      case (_, async: Async) =>
        host.threadPool.submit(new Runnable {
          if(ReactiveTransmittable.DEBUG) println(s"[${Thread.currentThread().getName}] $host processing async $async")
          override def run(): Unit = handleAsync(localReactive, endpoint, async)
        })
      case (requestId, request: Request) =>
        host.threadPool.submit(new Runnable {
          override def run(): Unit = {
            if(ReactiveTransmittable.DEBUG) println(s"[${Thread.currentThread().getName}] $host processing request $request")
            try {
              handleRequest(localReactive, endpoint, request).onComplete {
                // TODO exceptions should propatage back to the sender? but what if async?
                case Failure(e) =>
                  new Exception(s"$host failed processing request $request", e).printStackTrace()
                case Success(response) =>
                  if(ReactiveTransmittable.DEBUG) println(s"[${Thread.currentThread().getName}] $host replying $requestId: $response")
                  endpoint.send((requestId, response/*.toTuple*/))
              }(FullMVEngine.notWorthToMoveToTaskpool)
            } catch {
              // TODO exceptions should propatage back to the sender? but what if async?
              case t: Throwable => t.printStackTrace()
            }
          }
        })
      case (requestId, response: Response) =>
        if(ReactiveTransmittable.DEBUG) println(s"[${Thread.currentThread().getName}] $host processing response $response")
        handleResponse(requestId, response)
    }
  }

  override def send(value: R, remote: RemoteRef, endpoint: EndPointWithInfrastructure[Msg]): MessageWithInfrastructure[Msg] = {
    if(ReactiveTransmittable.DEBUG) println(s"[${Thread.currentThread().getName}] $host sending $value")
    endpoint.receive.notify (handleMessage(Left(value: ReSourciV[Pulse[P], FullMVStruct]), endpoint))
    (0L, UnitResponse/*.toTuple*/)
  }

  def doAsync(endpoint: EndPointWithInfrastructure[Msg], parameters: Async): Unit = {
    if(ReactiveTransmittable.DEBUG) println(s"[${Thread.currentThread().getName}] $host send async $parameters")
    endpoint.send((0L, parameters/*.toTuple*/))
  }

  def doRequest(endpoint: EndPointWithInfrastructure[Msg],  parameters: Request): Future[parameters.Response] = {
    val promise = Promise[parameters.Response]()
    @inline @tailrec def createRequest(): Long = {
      val requestId = ThreadLocalRandom.current().nextLong()
      if(requestId != 0 && requestTracker.putIfAbsent(requestId, promise) == null) requestId else createRequest()
    }
    val requestId = createRequest()
    if(ReactiveTransmittable.DEBUG) println(s"[${Thread.currentThread().getName}] $host send request $requestId: $parameters")
    endpoint.send((requestId, parameters/*.toTuple*/))
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

  def getKnownLocalTurnParameterInstance(guid: Host.GUID, endpoint: EndPointWithInfrastructure[Msg]): FullMVTurn = {
    host.getInstance(guid).get
  }

  def lookUpLocalTurnParameterInstance(bundle: TurnPushBundle, endpoint: EndPointWithInfrastructure[Msg]): FullMVTurn = {
    val (guid, phase, tree) = bundle
    val active = phase < TurnPhase.Completed
    // TODO ideally, the main turn and the predecessor turns should be tracked separately here. For the main turn,
    // both phase and predecessor tree must be subscribed. For the predecessor turns, a phase subscription is
    // sufficient, and not also subscribing predecessors might reduce the workload.
    if(active) {
      var toConnect = Set.empty[Host.GUID]
      val preds = tree.map { case (predGuid, predPhase) =>
        host.getCachedOrReceiveRemote(predGuid, {
          val predActive = predPhase < TurnPhase.Completed
          if (predActive) toConnect += predGuid
          (predActive, new FullMVTurnReflection(host, predGuid, predPhase, new FullMVTurnMirrorProxyToEndpoint(predGuid, endpoint)))
        }).instance
      }

      val instance = host.getCachedOrReceiveRemote(guid, {
        toConnect += guid
        val turn = new FullMVTurnReflection(host, guid, phase, new FullMVTurnMirrorProxyToEndpoint(guid, endpoint))
        turn.newPredecessors(preds)
        (true, turn)
      }) match {
        case Found(found) =>
          if (found.isInstanceOf[FullMVTurnReflection]) {
            val reflection = found.asInstanceOf[FullMVTurnReflectionProxyToEndpoint]
            reflection.newPhase(phase)
            reflection.newPredecessors(preds)
          }
          found
        case Instantiated(x) => x
      }
      if (toConnect.nonEmpty) doAsync(endpoint, AsyncAddReplicator(toConnect))
      instance
    } else {
      new FullMVTurnReflection(host, guid, phase, new FullMVTurnMirrorProxyToEndpoint(guid, endpoint))
    }
  }

  def localTurnReceiverInstance(guid: Host.GUID): Option[FullMVTurn] = {
    host.getInstance(guid)
  }

  def localTurnReflectionReceiverInstance(guid: Host.GUID): FullMVTurnReflection = {
    localTurnReceiverInstance(guid).asInstanceOf[FullMVTurnReflection]
  }

  val valuePersistency: Initializer.Param[Pulse[P]]

  override def receive(value: MessageWithInfrastructure[Msg], remote: RemoteRef, endpoint: EndPointWithInfrastructure[Msg]): R = {
    if(ReactiveTransmittable.DEBUG) println(s"[${Thread.currentThread().getName}] $host receiving a value")
    val turn = host.newTurn()
    turn.awaitAndSwitchPhase(TurnPhase.Executing)
    val state = turn.makeDerivedStructState(valuePersistency)
    val reflection = instantiate(state, turn)
    if(ReactiveTransmittable.DEBUG) println(s"[${Thread.currentThread().getName}] $host instantiating $reflection, requesting initialization starting at $turn")
    endpoint.receive.notify (handleMessage(Right(reflection), endpoint))
    doRequest(endpoint, Connect[Pluse[P]](bundle(turn))).foreach {
      case Initialize(initValues, maybeFirstFrame) =>
        if(ReactiveTransmittable.DEBUG) println(s"[${Thread.currentThread().getName}] $host received initialization package for $reflection")
        val reflectionInitValues = initValues.map{ case (mirrorTurn, v) => lookUpLocalTurnParameterInstance(mirrorTurn, endpoint) -> v }
        val reflectionMaybeFirstFrame = maybeFirstFrame.map(lookUpLocalTurnParameterInstance(_, endpoint))

        state.retrofitSinkFrames(reflectionInitValues.map(_._1), reflectionMaybeFirstFrame, +1)
        for((reflectionTurn, v) <- reflectionInitValues) reflection.buffer(reflectionTurn, v.toPulse)

        turn.ignite(reflection, Set.empty, valuePersistency.ignitionRequiresReevaluation)

        turn.awaitAndSwitchPhase(TurnPhase.Completed)
    }(executeInTaskPool)

    reflection
  }

  def instantiate(state: NodeVersionHistory[Pulse[P], FullMVTurn, ReSource[FullMVStruct], Reactive[FullMVStruct]], initTurn: FullMVTurn): ReactiveReflectionImpl[P] with R

  def handleAsync(localReactive: Either[ReSourciV[Pulse[P], FullMVStruct], ReactiveReflection[P]], endpoint: EndPointWithInfrastructure[Msg], message: Async): Unit = message match {
    case AsyncIncrementFrame(turn) =>
      localReactive.right.get.asyncIncrementFrame(lookUpLocalTurnParameterInstance(turn, endpoint))
    case AsyncDecrementFrame(turn) =>
      localReactive.right.get.asyncDecrementFrame(lookUpLocalTurnParameterInstance(turn, endpoint))
    case AsyncIncrementSupersedeFrame(turn, supersede) =>
      localReactive.right.get.asyncIncrementSupersedeFrame(lookUpLocalTurnParameterInstance(turn, endpoint), lookUpLocalTurnParameterInstance(supersede, endpoint))
    case AsyncDeframeReframe(turn, reframe) =>
      localReactive.right.get.asyncDeframeReframe(lookUpLocalTurnParameterInstance(turn, endpoint), lookUpLocalTurnParameterInstance(reframe, endpoint))
    case AsyncResolveUnchanged(turn) =>
      localReactive.right.get.asyncResolvedUnchanged(lookUpLocalTurnParameterInstance(turn, endpoint))
    case AsyncResolveUnchangedFollowFrame(turn, followFrame) =>
      localReactive.right.get.asyncResolvedUnchangedFollowFrame(lookUpLocalTurnParameterInstance(turn, endpoint), lookUpLocalTurnParameterInstance(followFrame, endpoint))
    case AsyncNewValue(turn, value) =>
      localReactive.right.get.asyncNewValue(lookUpLocalTurnParameterInstance(turn, endpoint), value.toPulse)
    case AsyncNewValueFollowFrame(turn, value, followFrame) =>
      localReactive.right.get.asyncNewValueFollowFrame(lookUpLocalTurnParameterInstance(turn, endpoint), value.toPulse, lookUpLocalTurnParameterInstance(followFrame, endpoint))

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

    case AsyncAddReplicator(receivers) =>
      doAsync(endpoint, AsyncAddReplicatorSyncData(receivers.map { receiver =>
        receiver -> (localTurnReceiverInstance(receiver) match {
          case Some(turn) =>
            val (predPhase, predTree) = turn.addReplicator(new FullMVTurnReflectionProxyToEndpoint(receiver, endpoint))
            (predPhase, sendTree(predTree))
          case None => (TurnPhase.Completed, CaseClassTransactionSpanningTreeNode((receiver, TurnPhase.Completed), Array.empty[CaseClassTransactionSpanningTreeNode[(Host.GUID, TurnPhase.Type)]]))
        })
      }.toMap))
    case AsyncAddReplicatorSyncData(syncedData) =>
      for((turn, (phase, tree)) <- syncedData) {
        val localReflection = localTurnReflectionReceiverInstance(turn)
        localReflection.newPhase(phase)
        // TODO this should be a phase-subscription-only connect, without a predecessor tree subscription.
        val preds = receivePredecessorTree(tree, endpoint)
        localReflection.newPredecessors(preds)
      }
  }

  private def sendTree(predTree: TransactionSpanningTreeNode[FullMVTurn]) = {
    predTree.map { predPred =>
      assert(predPred.host == host)
      (predPred.guid, predPred.phase)
    }
  }

  def receivePredecessorTree(tree: CaseClassTransactionSpanningTreeNode[(Host.GUID, TurnPhase.Type)], endpoint: EndPointWithInfrastructure[Msg]): CaseClassTransactionSpanningTreeNode[FullMVTurn] = {
    var toConnect = Set.empty[Host.GUID]
    val preds = tree.map { case (predGuid, predPhase) =>
      host.getCachedOrReceiveRemote(predGuid, {
        val active = predPhase < TurnPhase.Completed
        if (active) toConnect += predGuid
        (active, new FullMVTurnReflection(host, predGuid, predPhase, new FullMVTurnMirrorProxyToEndpoint(predGuid, endpoint)))
      }).instance
    }
    if(toConnect.nonEmpty) doAsync(endpoint, AsyncAddReplicator(toConnect))
    preds
  }

  def handleRequest(localReactive: Either[ReSourciV[Pulse[P], FullMVStruct], ReactiveReflection[P]], endpoint: EndPointWithInfrastructure[Msg], request: Request): Future[Response] = request match {
    case Connect(turn) =>
      val (initValues, maybeFirstFrame) = ReactiveMirror[Pulse[P]](localReactive.left.get, lookUpLocalTurnParameterInstance(turn, endpoint), new ReactiveReflectionProxyToEndpoint(endpoint), valuePersistency.isTransient, s"Mirror($endpoint)")
      Future.successful(Initialize(initValues.map { case (aTurn, value) =>
        (bundle(aTurn), Pluse.fromPulse(value))
      }, maybeFirstFrame.map(bundle)))
    case AddRemoteBranch(receiver, forPhase) =>
      val maybeTurn = localTurnReceiverInstance(receiver)
      assert(maybeTurn.isDefined, s"someone tried to revive $receiver, which should thus not have been possible to be deallocated")
      maybeTurn.get.addRemoteBranch(forPhase).map(_ => UnitResponse)(FullMVEngine.notWorthToMoveToTaskpool)
    case AcquirePhaseLockIfAtMost(receiver, phase) =>
      localTurnReceiverInstance(receiver) match {
        case Some(turn) => turn.acquirePhaseLockIfAtMost(phase).map(AcquirePhaseLockResponse)(FullMVEngine.notWorthToMoveToTaskpool)
        case None => Future.successful(AcquirePhaseLockResponse(TurnPhase.Completed))
      }
    case MaybeNewReachableSubtree(receiver, attachBelow, spanningSubTreeRoot) =>
      val maybeTurn = localTurnReceiverInstance(receiver)
      assert(maybeTurn.isDefined, s"someone tried to share possible transitive predecessors with $receiver, which should thus not have been possible to be deallocated")
      maybeTurn.get.maybeNewReachableSubtree(getKnownLocalTurnParameterInstance(attachBelow, endpoint), receivePredecessorTree(spanningSubTreeRoot, endpoint)).map(_ => UnitResponse)(FullMVEngine.notWorthToMoveToTaskpool)
    case AddPredecessor(receiver, predecessorTree) =>
      val maybeTurn = localTurnReceiverInstance(receiver)
      assert(maybeTurn.isDefined, s"someone tried to add predecessors on turn $receiver, which should thus not have been possible to be deallocated")
      maybeTurn.get.addPredecessor(receivePredecessorTree(predecessorTree, endpoint)).map(_ => UnitResponse)(FullMVEngine.notWorthToMoveToTaskpool)
    case NewSuccessor(receiver, successor) =>
      localTurnReceiverInstance(receiver) match {
        case Some(turn) => turn.newSuccessor(getKnownLocalTurnParameterInstance(successor, endpoint)).map(_ => UnitResponse)(FullMVEngine.notWorthToMoveToTaskpool)
        case None => Future.successful(UnitResponse) // predecessor was concurrently deallocated and thus just won't message successor any longer and we can just ignore this call.
      }
    case TurnGetLockedRoot(receiver) =>
      localTurnReceiverInstance(receiver) match {
        case Some(turn) => turn.getLockedRoot.map(MaybeLockResponse)(FullMVEngine.notWorthToMoveToTaskpool)
        case None => Future.successful(MaybeLockResponse(None))
      }
    case TurnTryLock(receiver) =>
      localTurnReceiverInstance(receiver) match {
        case Some(turn) => turn.remoteTryLock().map {
          case Locked(lock) =>
            assert(lock.host == host)
            TurnLockedResponse(lock.guid)
          case Blocked => TurnBlockedResponse
          case Deallocated => TurnDeallocatedResponse
        }(FullMVEngine.notWorthToMoveToTaskpool)
        case None => Future.successful(TurnDeallocatedResponse)
      }
    case TurnTrySubsume(receiver, lockedNewParent) =>
      localTurnReceiverInstance(receiver) match {
        case Some(turn) => turn.remoteTrySubsume(lookUpLocalLockParameterInstanceWithReference(lockedNewParent, endpoint)).map {
          case Successful => TurnSuccessfulResponse
          case Blocked => TurnBlockedResponse
          case Deallocated => TurnDeallocatedResponse
        }(FullMVEngine.notWorthToMoveToTaskpool)
        case None => Future.successful(TurnDeallocatedResponse)
      }
    case NewPredecessors(receiver, predecessors) =>
      localTurnReflectionReceiverInstance(receiver).newPredecessors(receivePredecessorTree(predecessors, endpoint)).map(_ => UnitResponse)(FullMVEngine.notWorthToMoveToTaskpool)
    case NewPhase(receiver, phase) =>
      localTurnReflectionReceiverInstance(receiver).newPhase(phase).map(_ => UnitResponse)(FullMVEngine.notWorthToMoveToTaskpool)


    case LockGetLockedRoot(receiver) =>
      localLockReceiverInstance(receiver) match {
        case Some(lock) => lock.getLockedRoot.map(MaybeLockResponse)(FullMVEngine.notWorthToMoveToTaskpool)
        case None => Future.failed(new AssertionError(s"query for locked root on deallocated turn $receiver"))
      }
    case LockTryLock(receiver) =>
      localLockReceiverInstance(receiver) match {
        case Some(lock) =>
          lock.remoteTryLock().map {
            case RemoteLocked(newParent) =>
              assert(newParent.host == host)
              LockLockedResponse(newParent.guid)
            case RemoteBlocked(newParent) =>
              assert(newParent.host == host)
              LockBlockedResponse(newParent.guid)
            case RemoteGCd => LockDeallocatedResponse
          }(FullMVEngine.notWorthToMoveToTaskpool)
        case None => Future.successful(LockDeallocatedResponse)
      }
    case LockTrySubsume(receiver, lockedNewParent) =>
      localLockReceiverInstance(receiver) match {
        case Some(lock) =>
          lock.remoteTrySubsume(lookUpLocalLockParameterInstanceWithReference(lockedNewParent, endpoint)).map {
            case RemoteSubsumed => LockSuccessfulResponse
            case RemoteBlocked(newParent) =>
              assert(newParent.host == host)
              LockBlockedResponse(newParent.guid)
            case RemoteGCd => LockDeallocatedResponse
          }(FullMVEngine.notWorthToMoveToTaskpool)
        case None => Future.successful(LockDeallocatedResponse)
      }
    case otherwise =>
      throw new AssertionError("undefined message : "+otherwise)
  }

  class FullMVTurnMirrorProxyToEndpoint(val guid: Host.GUID, endpoint: EndPointWithInfrastructure[Msg]) extends FullMVTurnProxy {
    override def addRemoteBranch(forPhase: TurnPhase.Type): Future[Unit] = {
      doRequest(endpoint, AddRemoteBranch(guid, forPhase)).map(_ => ())(FullMVEngine.notWorthToMoveToTaskpool)
    }

    override def asyncRemoteBranchComplete(forPhase: TurnPhase.Type): Unit = {
      doAsync(endpoint, AsyncRemoteBranchComplete(guid, forPhase))
    }

    override def acquirePhaseLockIfAtMost(maxPhase: Type): Future[TurnPhase.Type] = {
      doRequest(endpoint, AcquirePhaseLockIfAtMost(guid, maxPhase)).map {
        case AcquirePhaseLockResponse(phase) => phase
      }(executeInTaskPool)
    }

    override def addPredecessor(tree: TransactionSpanningTreeNode[FullMVTurn]): Future[Unit] = {
      doRequest(endpoint, AddPredecessor(guid, sendTree(tree))).map(_ => ())(FullMVEngine.notWorthToMoveToTaskpool)
    }
    override def asyncReleasePhaseLock(): Unit = {
      doAsync(endpoint, AsyncReleasePhaseLock(guid))
    }
    override def maybeNewReachableSubtree(attachBelow: FullMVTurn, spanningSubTreeRoot: TransactionSpanningTreeNode[FullMVTurn]): Future[Unit] = {
      assert(attachBelow.host == host)
      doRequest(endpoint, MaybeNewReachableSubtree(guid, attachBelow.guid, sendTree(spanningSubTreeRoot))).map(_ => ())(FullMVEngine.notWorthToMoveToTaskpool)
    }
    override def newSuccessor(successor: FullMVTurn): Future[Unit] = {
      assert(successor.host == host)
      doRequest(endpoint, NewSuccessor(guid, successor.guid)).map(_ => ())(FullMVEngine.notWorthToMoveToTaskpool)
    }

    override def getLockedRoot: Future[Option[Host.GUID]] = {
      doRequest(endpoint, TurnGetLockedRoot(guid)).map {
        case MaybeLockResponse(maybeRoot) =>
          maybeRoot
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
      assert(lockedNewParent.host == host.lockHost)
      doRequest(endpoint, TurnTrySubsume(guid, lockedNewParent.guid)).map {
        case TurnSuccessfulResponse => Successful
        case TurnBlockedResponse => Blocked
        case TurnDeallocatedResponse => Deallocated
      }(executeInTaskPool)
    }
  }

  class FullMVTurnReflectionProxyToEndpoint(val guid: Host.GUID, endpoint: EndPointWithInfrastructure[Msg]) extends FullMVTurnReflectionProxy {
    override def newPredecessors(predecessors: TransactionSpanningTreeNode[FullMVTurn]): Future[Unit] = {
      doRequest(endpoint, NewPredecessors(guid, sendTree(predecessors))).map(_ => ())(FullMVEngine.notWorthToMoveToTaskpool)
    }

    override def newPhase(phase: TurnPhase.Type): Future[Unit] = {
      doRequest(endpoint, NewPhase(guid, phase)).map(_ => ())(FullMVEngine.notWorthToMoveToTaskpool)
    }
  }

  class SubsumableLockProxyToEndpoint(val guid: Host.GUID, endpoint: EndPointWithInfrastructure[Msg]) extends SubsumableLockProxy {
    override def remoteAsyncUnlock(): Unit = {
      doAsync(endpoint, LockAsyncUnlock(guid))
    }
    override def getLockedRoot: Future[Option[Host.GUID]] = {
      doRequest(endpoint, LockGetLockedRoot(guid)).map {
        case MaybeLockResponse(maybeRoot) =>
          maybeRoot
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
      assert(lockedNewParent.host == host.lockHost)
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
