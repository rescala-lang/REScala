package rescala.fullmv.transmitter

import java.io.{ByteArrayInputStream, ByteArrayOutputStream, ObjectInputStream, ObjectOutputStream}
import java.util.concurrent.{ConcurrentHashMap, Executor, ThreadLocalRandom}

import rescala.core.Reactive
import rescala.core._
import rescala.fullmv.TurnPhase.Type
import rescala.fullmv.mirrors.Host.GUID
import rescala.fullmv.mirrors._
import rescala.fullmv.sgt.synchronization.SubsumableLock
import rescala.fullmv.sgt.synchronization.SubsumableLock.TryLockResult
import rescala.fullmv._
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

  val notWorthToMoveToTaskpool: ExecutionContextExecutor = ExecutionContext.fromExecutor(new Executor{
    override def execute(command: Runnable): Unit = command.run()
  })

  type EndPointWithInfrastructure[T] = Endpoint[MessageWithInfrastructure[T], MessageWithInfrastructure[T]]
  type MessageWithInfrastructure[T] = (Long, T)

  type Msg[+T] = (String, Host.GUID, Host.GUID, Option[T], Seq[(Host.GUID, T)], TurnPhase.Type, CaseClassTransactionSpanningTreeNode[Host.GUID], Boolean, Long, Seq[Host.GUID])
  def allEmpty[T](name: String): Msg[T] = (name, Host.dummyGuid, Host.dummyGuid, None, Seq.empty, TurnPhase.dummy, CaseClassTransactionSpanningTreeNode[Host.GUID](Host.dummyGuid, Set.empty), false, 0L, Seq.empty)
  val ASYNC_REQUEST = 0

  sealed trait Message[+P] {
    def toTuple: Msg[P] = this match {
      case UnitResponse => allEmpty("UnitResponse")
      case Connect(turn) => allEmpty("Connect").copy(_2 = turn)
      case Initialize(initValues, maybeFirstFrame) => allEmpty("Initialize").copy(_2 = maybeFirstFrame.getOrElse(Host.dummyGuid), _5 = initValues, _8 = maybeFirstFrame.isDefined)
      case AsyncIncrementFrame(turn) => allEmpty("AsyncIncrementFrame").copy(_2 = turn)
      case AsyncIncrementSupersedeFrame(turn, supersede) => allEmpty("AsyncIncrementSupersedeFrame").copy(_2 = turn, _3 = supersede)
      case AsyncResolveUnchanged(turn) => allEmpty("AsyncResolveUnchanged").copy(_2 = turn)
      case AsyncResolveUnchangedFollowFrame(turn, followFrame) => allEmpty("AsyncResolveUnchangedFollowFrame").copy(_2 = turn, _3 = followFrame)
      case AsyncNewValue(turn, value) => allEmpty("AsyncNewValue").copy(_2 = turn, _4 = Some(value))
      case AsyncNewValueFollowFrame(turn, value, followFrame) => allEmpty("AsyncNewValueFollowFrame").copy(_2 = turn, _3 = followFrame, _4 = Some(value))
      case AddReplicator(turn) => allEmpty("AddReplicator").copy(_2 = turn)
      case AddReplicatorResponse(initPhase, initPreds) => allEmpty("AddReplicatorResponse").copy(_6 = initPhase, _10 = initPreds)
      case AddRemoteBranch(turn, forPhase) => allEmpty("AddRemoteBranch").copy(_2 = turn, _6 = forPhase)
      case AsyncRemoteBranchComplete(turn, forPhase) => allEmpty("AsyncRemoteBranchComplete").copy(_2 = turn, _6 = forPhase)
      case AcquirePhaseLockAndGetEstablishmentBundle(turn) => allEmpty("AcquirePhaseLockAndGetEstablishmentBundle").copy(_2 = turn)
      case AcquirePhaseLockAndGetEstablishmentBundleResponse(phase, predecessorTree) => allEmpty("AcquirePhaseLockAndGetEstablishmentBundleResponse").copy(_6 = phase, _7 = predecessorTree)
      case AddPredecessorAndReleasePhaseLock(turn, predecessorTree) => allEmpty("AddPredecessorAndReleasePhaseLock").copy(_2 = turn, _7 = predecessorTree)
      case AsyncReleasePhaseLock(turn) => allEmpty("AsyncReleasePhaseLock").copy(_2 = turn)
      case MaybeNewReachableSubtree(turn, attachBelow, spanningTree) => allEmpty("MaybeNewReachableSubtree").copy(_2 = turn, _3 = attachBelow, _7 = spanningTree)
      case NewSuccessor(turn, successor) => allEmpty("NewSuccessor").copy(_2 = turn, _3 = successor)
      case TurnGetLockedRoot(turn) => allEmpty("TurnGetLockedRoot").copy(_2 = turn)
      case LockGetLockedRoot(lock) => allEmpty("LockGetLockedRoot").copy(_2 = lock)
      case TurnTryLock(turn) => allEmpty("TurnTryLock").copy(_2 = turn)
      case LockTryLock(lock) => allEmpty("LockTryLock").copy(_2 = lock)
      case TryLockResponse(success, newParent) => allEmpty("TryLockResponse").copy(_2 = newParent, _8 = success)
      case TurnLock(turn) => allEmpty("TurnLock").copy(_2 = turn)
      case LockLock(lock) => allEmpty("LockLock").copy(_2 = lock)
      case TurnSpinOnce(turn, backoff) => allEmpty("TurnSpinOnce").copy(_2 = turn, _9 = backoff)
      case LockSpinOnce(lock, backoff) => allEmpty("LockSpinOnce").copy(_2 = lock, _9 = backoff)
      case LockResponse(newParent) => allEmpty("LockResponse").copy(_2 = newParent)
      case TurnTrySubsume(turn, lockedNewParent) => allEmpty("TurnTrySubsume").copy(_2 = turn, _3 = lockedNewParent)
      case LockTrySubsume(lock, lockedNewParent) => allEmpty("LockTrySubsume").copy(_2 = lock, _3 = lockedNewParent)
      case MaybeLockResponse(newParentIfFailed) => allEmpty("MaybeLockResponse").copy(_2 = newParentIfFailed.getOrElse(Host.dummyGuid), _8 = newParentIfFailed.isDefined)
      case TurnSubsume(turn, lockedNewParent) => allEmpty("TurnSubsume").copy(_2 = turn, _3 = lockedNewParent)
      case LockSubsume(lock, lockedNewParent) => allEmpty("LockSubsume").copy(_2 = lock, _3 = lockedNewParent)
      case TurnUnlock(turn) => allEmpty("TurnUnlock").copy(_2 = turn)
      case LockUnlock(lock) => allEmpty("LockUnlock").copy(_2 = lock)
      case NewPredecessors(turn, newPredecessors) => allEmpty("NewPredecessors").copy(_2 = turn, _10 = newPredecessors)
      case NewPhase(turn, phase) => allEmpty("NewPhase").copy(_2 = turn, _6 = phase)
    }
  }
  object Message {
    def fromTuple[P](tuple: Msg[P]): Message[P] = tuple match {
      case ("UnitResponse", _, _, _, _, _, _, _, _, _) => UnitResponse
      case ("Connect", turn, _, _, _, _, _, _, _, _) => Connect(turn)
      case ("Initialize", mbff, _, _, initValues, _, _, mb, _, _) => Initialize(initValues, if(mb) Some(mbff) else None)
      case ("AsyncIncrementFrame", turn, _, _, _, _, _, _, _, _) => AsyncIncrementFrame(turn)
      case ("AsyncIncrementSupersedeFrame", turn, supersede, _, _, _, _, _, _, _) => AsyncIncrementSupersedeFrame(turn, supersede)
      case ("AsyncResolveUnchanged", turn, _, _, _, _, _, _, _, _) => AsyncResolveUnchanged(turn)
      case ("AsyncResolveUnchangedFollowFrame", turn, followFrame, _, _, _, _, _, _, _) => AsyncResolveUnchangedFollowFrame(turn, followFrame)
      case ("AsyncNewValue", turn, _, Some(value), _, _, _, _, _, _) => AsyncNewValue(turn, value)
      case ("AsyncNewValueFollowFrame", turn, followFrame, Some(value), _, _, _, _, _, _) => AsyncNewValueFollowFrame(turn, value, followFrame)
      case ("AddReplicator", turn, _, _, _, _, _, _, _, _) => AddReplicator(turn)
      case ("AddReplicatorResponse", _, _, _, _, initPhase, _, _, _, initPreds) => AddReplicatorResponse(initPhase, initPreds)
      case ("AddRemoteBranch", turn, _, _, _, forPhase, _, _, _, _) => AddRemoteBranch(turn, forPhase)
      case ("AsyncRemoteBranchComplete", turn, _, _, _, forPhase, _, _, _, _)=> AsyncRemoteBranchComplete(turn, forPhase)
      case ("AcquirePhaseLockAndGetEstablishmentBundle", turn, _, _, _, _, _, _, _, _) => AcquirePhaseLockAndGetEstablishmentBundle(turn)
      case ("AcquirePhaseLockAndGetEstablishmentBundleResponse", _, _, _, _, phase, predecessorTree, _, _, _) => AcquirePhaseLockAndGetEstablishmentBundleResponse(phase, predecessorTree)
      case ("AddPredecessorAndReleasePhaseLock", turn, _, _, _, _, predecessorTree, _, _, _) => AddPredecessorAndReleasePhaseLock(turn, predecessorTree)
      case ("AsyncReleasePhaseLock", turn, _, _, _, _, _, _, _, _) => AsyncReleasePhaseLock(turn)
      case ("MaybeNewReachableSubtree", turn, attachBelow, _, _, _, spanningTree, _, _, _) => MaybeNewReachableSubtree(turn, attachBelow, spanningTree)
      case ("NewSuccessor", turn, successor, _, _, _, _, _, _, _) => NewSuccessor(turn, successor)
      case ("TurnGetLockedRoot", turn, _, _, _, _, _, _, _, _) => TurnGetLockedRoot(turn)
      case ("LockGetLockedRoot", lock, _, _, _, _, _, _, _, _) => LockGetLockedRoot(lock)
      case ("TurnTryLock", turn, _, _, _, _, _, _, _, _) => TurnTryLock(turn)
      case ("LockTryLock", lock, _, _, _, _, _, _, _, _) => LockTryLock(lock)
      case ("TryLockResponse", newParent, _, _, _, _, _, success, _, _) => TryLockResponse(success, newParent)
      case ("TurnLock", turn, _, _, _, _, _, _, _, _) => TurnLock(turn)
      case ("LockLock", lock, _, _, _, _, _, _, _, _) => LockLock(lock)
      case ("TurnSpinOnce", turn, _, _, _, _, _, _, backoff, _) => TurnSpinOnce(turn, backoff)
      case ("LockSpinOnce", lock, _, _, _, _, _, _, backoff, _) => LockSpinOnce(lock, backoff)
      case ("LockResponse", newParent, _, _, _, _, _, _, _, _) => LockResponse(newParent)
      case ("TurnTrySubsume", turn, lockedNewParent, _, _, _, _, _, _, _) => TurnTrySubsume(turn, lockedNewParent)
      case ("LockTrySubsume", lock, lockedNewParent, _, _, _, _, _, _, _) => LockTrySubsume(lock, lockedNewParent)
      case ("MaybeLockResponse", mbnp, _, _, _, _, _, mb, _, _) => MaybeLockResponse(if(mb) Some(mbnp) else None)
      case ("TurnSubsume", turn, lockedNewParent, _, _, _, _, _, _, _) => TurnSubsume(turn, lockedNewParent)
      case ("LockSubsume", lock, lockedNewParent, _, _, _, _, _, _, _) => LockSubsume(lock, lockedNewParent)
      case ("TurnUnlock", turn, _, _, _, _, _, _, _, _) => TurnUnlock(turn)
      case ("LockUnlock", lock, _, _, _, _, _, _, _, _) => LockUnlock(lock)
      case ("NewPredecessors", turn, _, _, _, _, _, _, _, newPredecessors) => NewPredecessors(turn, newPredecessors)
      case ("NewPhase", turn, _, _, _, phase, _, _, _, _) => NewPhase(turn, phase)
      case otherwise =>
        val e = new AssertionError("Unrecognized message: " + otherwise)
        e.printStackTrace()
        throw e
    }
  }
  sealed trait Async[+P] extends Message[P]
  sealed trait Request[+P] extends Message[P] {
    type Response <: ReactiveTransmittable.this.Response[P]
  }
  sealed trait Response[+P] extends Message[P]
  case object UnitResponse extends Response[Nothing]

  case class Connect[P](turn: Host.GUID) extends Request[P]{ override type Response = Initialize[P] }
  case class Initialize[P](initValues: Seq[(Host.GUID, P)], maybeFirstFrame: Option[Host.GUID]) extends Response[P]

  case class AsyncIncrementFrame(turn: Host.GUID) extends Async[Nothing]
  case class AsyncIncrementSupersedeFrame(turn: Host.GUID, supersede: Host.GUID) extends Async[Nothing]
  case class AsyncResolveUnchanged(turn: Host.GUID) extends Async[Nothing]
  case class AsyncResolveUnchangedFollowFrame(turn: Host.GUID, followFrame: Host.GUID) extends Async[Nothing]
  case class AsyncNewValue[P](turn: Host.GUID, value: P) extends Async[P]
  case class AsyncNewValueFollowFrame[P](turn: Host.GUID, value: P, followFrame: Host.GUID) extends Async[P]

  case class AddReplicator(turn: Host.GUID) extends Request[Nothing] { override type Response = AddReplicatorResponse }
  case class AddReplicatorResponse(initPhase: TurnPhase.Type, initPreds: Seq[Host.GUID]) extends Response[Nothing]
  case class AddRemoteBranch(turn: Host.GUID, forPhase: TurnPhase.Type) extends Request[Nothing] { override type Response = UnitResponse.type }
  case class AsyncRemoteBranchComplete(turn: Host.GUID, forPhase: TurnPhase.Type) extends Async[Nothing]
  case class AcquirePhaseLockAndGetEstablishmentBundle(turn: Host.GUID) extends Request[Nothing]{ override type Response = AcquirePhaseLockAndGetEstablishmentBundleResponse }
  case class AcquirePhaseLockAndGetEstablishmentBundleResponse(phase: TurnPhase.Type, predecessorTree: CaseClassTransactionSpanningTreeNode[Host.GUID]) extends Response[Nothing]
  case class AddPredecessorAndReleasePhaseLock(turn: Host.GUID, predecessorTree: CaseClassTransactionSpanningTreeNode[Host.GUID]) extends Request[Nothing]{ override type Response = UnitResponse.type }
  case class AsyncReleasePhaseLock(turn: Host.GUID) extends Async[Nothing]
  case class MaybeNewReachableSubtree(turn: Host.GUID, attachBelow: Host.GUID, spanningTree: CaseClassTransactionSpanningTreeNode[Host.GUID]) extends Request[Nothing]{ override type Response = UnitResponse.type }
  case class NewSuccessor(turn: Host.GUID, successor: Host.GUID) extends Request[Nothing]{ override type Response = UnitResponse.type }

  case class TurnGetLockedRoot(turn: Host.GUID) extends Request[Nothing]{ override type Response = MaybeLockResponse }
  case class LockGetLockedRoot(lock: Host.GUID) extends Request[Nothing]{ override type Response = MaybeLockResponse }
  case class TurnTryLock(turn: Host.GUID) extends Request[Nothing]{ override type Response = TryLockResponse }
  case class LockTryLock(lock: Host.GUID) extends Request[Nothing]{ override type Response = TryLockResponse }
  case class TryLockResponse(success: Boolean, newParent: Host.GUID) extends Response[Nothing]
  case class TurnLock(turn: Host.GUID) extends Request[Nothing]{ override type Response = LockResponse }
  case class LockLock(lock: Host.GUID) extends Request[Nothing]{ override type Response = LockResponse }
  case class TurnSpinOnce(turn: Host.GUID, backoff: Long) extends Request[Nothing]{ override type Response = LockResponse }
  case class LockSpinOnce(lock: Host.GUID, backoff: Long) extends Request[Nothing]{ override type Response = LockResponse }
  case class LockResponse(newParent: Host.GUID) extends Response[Nothing]
  case class TurnTrySubsume(turn: Host.GUID, lockedNewParent: Host.GUID) extends Request[Nothing]{ override type Response = MaybeLockResponse }
  case class LockTrySubsume(lock: Host.GUID, lockedNewParent: Host.GUID) extends Request[Nothing]{ override type Response = MaybeLockResponse }
  case class MaybeLockResponse(newParentIfFailed: Option[Host.GUID]) extends Response[Nothing]
  case class TurnSubsume(turn: Host.GUID, lockedNewParent: Host.GUID) extends Request[Nothing]{ override type Response = UnitResponse.type }
  case class LockSubsume(lock: Host.GUID, lockedNewParent: Host.GUID) extends Request[Nothing]{ override type Response = UnitResponse.type }
  case class TurnUnlock(turn: Host.GUID) extends Request[Nothing]{ override type Response = LockResponse }
  case class LockUnlock(lock: Host.GUID) extends Request[Nothing]{ override type Response = LockResponse }

  case class NewPredecessors(turn: Host.GUID, newPredecessors: Seq[Host.GUID]) extends Request[Nothing]{ override type Response = UnitResponse.type }
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

  implicit def signalTransmittable[P, S](implicit host: FullMVEngine, messageTransmittable: Transmittable[MessageWithInfrastructure[Msg[Pluse[P]]], S, MessageWithInfrastructure[Msg[Pluse[P]]]], serializable: Serializable[S]): Transmittable[Signal[P, FullMVStruct], S, Signal[P, FullMVStruct]] = new ReactiveTransmittable[P, Signal[P, FullMVStruct], S] {
    override def instantiate(state: NodeVersionHistory[Pulse[P], FullMVTurn, ReSource[FullMVStruct], Reactive[FullMVStruct]], initTurn: FullMVTurn) =
      new ReactiveReflectionImpl[P](host, None, state, "SignalReflection") with Signal[P, FullMVStruct] {
        override def disconnect()(implicit engine: Engine[FullMVStruct]): Unit = ???
      }
    override val valuePersistency: ValuePersistency[Pulse[P]] = ValuePersistency.DerivedSignal[P]
  }
  implicit def eventTransmittable[P, S](implicit host: FullMVEngine, messageTransmittable: Transmittable[MessageWithInfrastructure[Msg[Pluse[P]]], S, MessageWithInfrastructure[Msg[Pluse[P]]]], serializable: Serializable[S]): Transmittable[Event[P, FullMVStruct], S, Event[P, FullMVStruct]] = new ReactiveTransmittable[P, Event[P, FullMVStruct], S] {
    override def instantiate(state: NodeVersionHistory[Pulse[P], FullMVTurn, ReSource[FullMVStruct], Reactive[FullMVStruct]], initTurn: FullMVTurn) =
      new ReactiveReflectionImpl[P](host, Some(initTurn), state, "EventReflection") with Event[P, FullMVStruct] {
        override def disconnect()(implicit engine: Engine[FullMVStruct]): Unit = ???
      }
    override val valuePersistency: ValuePersistency[Pulse[P]] = ValuePersistency.Event[P]
  }
}

abstract class ReactiveTransmittable[P, R <: ReSourciV[Pulse[P], FullMVStruct], S](implicit val host: FullMVEngine, messageTransmittable: Transmittable[ReactiveTransmittable.MessageWithInfrastructure[ReactiveTransmittable.Msg[ReactiveTransmittable.Pluse[P]]], S, ReactiveTransmittable.MessageWithInfrastructure[ReactiveTransmittable.Msg[ReactiveTransmittable.Pluse[P]]]], serializable: Serializable[S]) extends PushBasedTransmittable[R, ReactiveTransmittable.MessageWithInfrastructure[ReactiveTransmittable.Msg[ReactiveTransmittable.Pluse[P]]], S, ReactiveTransmittable.MessageWithInfrastructure[ReactiveTransmittable.Msg[ReactiveTransmittable.Pluse[P]]], R] {
  import ReactiveTransmittable._
  type Msg = ReactiveTransmittable.Msg[Pluse[P]]
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
    (msg._1, Message.fromTuple(msg._2)) match {
      case (_, async: Async) =>
        host.threadPool.submit(new Runnable {
          if(ReactiveTransmittable.DEBUG) println(s"[${Thread.currentThread().getName}] $host processing async $async")
          override def run(): Unit = handleAsync(localReactive, endpoint, async)
        })
      case (requestId, request: Request) =>
        host.threadPool.submit(new Runnable {
          override def run(): Unit = {
            if(ReactiveTransmittable.DEBUG) println(s"[${Thread.currentThread().getName}] $host processing request $request")
            handleRequest(localReactive, endpoint, request).onComplete {
              // TODO better exception propagation
              case Failure(e) =>
                new Exception(s"$host failed processing request $request", e).printStackTrace()
              case Success(response) =>
                if(ReactiveTransmittable.DEBUG) println(s"[${Thread.currentThread().getName}] $host replying $requestId: $response")
                endpoint.send((requestId, response.toTuple))
            }(ReactiveTransmittable.notWorthToMoveToTaskpool)
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
    (0L, UnitResponse.toTuple)
  }

  def doAsync(endpoint: EndPointWithInfrastructure[Msg], parameters: Async): Unit = {
    if(ReactiveTransmittable.DEBUG) println(s"[${Thread.currentThread().getName}] $host send async $parameters")
    endpoint.send((0L, parameters.toTuple))
  }

  def doRequest(endpoint: EndPointWithInfrastructure[Msg],  parameters: Request): Future[parameters.Response] = {
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

  def localLockInstance(guid: Host.GUID, endpoint: EndPointWithInfrastructure[Msg]): SubsumableLock = {
    host.lockHost.getCachedOrReceiveRemote(guid) { doCache =>
      if(ReactiveTransmittable.DEBUG) println(s"[${Thread.currentThread().getName}] $host connecting lock $guid")
      val instance = new SubsumableLockReflection(host.lockHost, guid, new SubsumableLockMirrorProxyToEndpoint(guid, endpoint))
      doCache(instance)
      instance
    }
  }

  def localTurnInstance0(guid: Host.GUID, endpoint: EndPointWithInfrastructure[Msg]): FullMVTurn = {
    host.getCachedOrReceiveRemote(guid) { doCache =>
      if(ReactiveTransmittable.DEBUG) println(s"[${Thread.currentThread().getName}] $host connecting turn $guid")
      val instance = new FullMVTurnReflection(host, guid, new FullMVTurnMirrorProxyToEndpoint(guid, endpoint), host.timeout)
      doCache(instance)
      val AddReplicatorResponse(initPhase, initPreds) =  Await.result(doRequest(endpoint, AddReplicator(guid)), host.timeout)
      instance.newPredecessors(initPreds)
      instance.newPhase(initPhase)
      instance
    }
  }
  def localTurnInstance(guid: Host.GUID, endpoint: EndPointWithInfrastructure[Msg]): FullMVTurn = {
    val turn = localTurnInstance0(guid, endpoint)
    // TODO there might be a cleverer way to do this..
    turn.awaitPhase(TurnPhase.Initialized + 1)
    turn
  }
  def localTurnReflection(guid: Host.GUID, endpoint: EndPointWithInfrastructure[Msg]): FullMVTurnReflection = {
    localTurnInstance0(guid, endpoint).asInstanceOf[FullMVTurnReflection]
  }

  val valuePersistency: ValuePersistency[Pulse[P]]

  override def receive(value: MessageWithInfrastructure[Msg], remote: RemoteRef, endpoint: EndPointWithInfrastructure[Msg]): R = {
    if(ReactiveTransmittable.DEBUG) println(s"[${Thread.currentThread().getName}] $host receiving a value")
    val turn = host.newTurn()
    turn.awaitAndSwitchPhase(TurnPhase.Executing)
    val state = turn.makeDerivedStructState(valuePersistency)
    val reflection = instantiate(state, turn)
    if(ReactiveTransmittable.DEBUG) println(s"[${Thread.currentThread().getName}] $host instantiating $reflection, requesting initialization starting at $turn")
    endpoint.receive.notify (handleMessage(Right(reflection), endpoint))
    doRequest(endpoint, Connect[Pluse[P]](turn.guid)).foreach {
      case Initialize(initValues, maybeFirstFrame) =>
        if(ReactiveTransmittable.DEBUG) println(s"[${Thread.currentThread().getName}] $host received initialization package for $reflection")
        val reflectionInitValues = initValues.map{ case (mirrorTurn, v) => localTurnInstance(mirrorTurn, endpoint) -> v }
        val reflectionMaybeFirstFrame = maybeFirstFrame.map(localTurnInstance(_, endpoint))

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
      localReactive.right.get.asyncIncrementFrame(localTurnInstance(turn, endpoint))
    case AsyncIncrementSupersedeFrame(turn, supersede) =>
      localReactive.right.get.asyncIncrementSupersedeFrame(localTurnInstance(turn, endpoint), localTurnInstance(supersede, endpoint))
    case AsyncResolveUnchanged(turn) =>
      localReactive.right.get.asyncResolvedUnchanged(localTurnInstance(turn, endpoint))
    case AsyncResolveUnchangedFollowFrame(turn, followFrame) =>
      localReactive.right.get.asyncResolvedUnchangedFollowFrame(localTurnInstance(turn, endpoint), localTurnInstance(followFrame, endpoint))
    case AsyncNewValue(turn, value) =>
      localReactive.right.get.asyncNewValue(localTurnInstance(turn, endpoint), value.toPulse)
    case AsyncNewValueFollowFrame(turn, value, followFrame) =>
      localReactive.right.get.asyncNewValueFollowFrame(localTurnInstance(turn, endpoint), value.toPulse, localTurnInstance(followFrame, endpoint))

    case AsyncRemoteBranchComplete(receiver, forPhase) =>
      localTurnInstance(receiver, endpoint).asyncRemoteBranchComplete(forPhase)
    case AsyncReleasePhaseLock(receiver) =>
      localTurnInstance(receiver, endpoint).asyncReleasePhaseLock()
  }

  def handleRequest(localReactive: Either[ReSourciV[Pulse[P], FullMVStruct], ReactiveReflection[P]], endpoint: EndPointWithInfrastructure[Msg], request: Request): Future[Response] = request match {
    case Connect(turn) =>
      val (initValues, maybeFirstFrame) = ReactiveMirror[Pulse[P]](localReactive.left.get, localTurnInstance(turn, endpoint), new ReactiveReflectionProxyToEndpoint(endpoint), valuePersistency.isTransient, s"Mirror($endpoint)")
      Future.successful(Initialize(initValues.map { case (aTurn, value) =>
        assert(aTurn.host == host)
        (aTurn.guid, Pluse.fromPulse(value))
      }, maybeFirstFrame.map(_.guid)))
    case AddReplicator(receiver) =>
      val (initPhase, initPreds) = localTurnInstance(receiver, endpoint).addReplicator(new FullMVTurnReflectionProxyToEndpoint(receiver, endpoint))
      Future.successful(AddReplicatorResponse(initPhase, initPreds))
    case AddRemoteBranch(receiver, forPhase) =>
      localTurnInstance(receiver, endpoint).addRemoteBranch(forPhase).map(_ => UnitResponse)(notWorthToMoveToTaskpool)
    case AcquirePhaseLockAndGetEstablishmentBundle(receiver) =>
      localTurnInstance(receiver, endpoint).acquirePhaseLockAndGetEstablishmentBundle().map {
        case (phase, predecessorTree) => AcquirePhaseLockAndGetEstablishmentBundleResponse(phase, predecessorTree.map { turn =>
          assert(turn.host == host)
          turn.guid
        })
      }(ReactiveTransmittable.notWorthToMoveToTaskpool)
    case AddPredecessorAndReleasePhaseLock(receiver, predecessorSpanningTree) =>
      localTurnInstance(receiver, endpoint).addPredecessorAndReleasePhaseLock(predecessorSpanningTree.map(localTurnInstance(_, endpoint))).map(_ => UnitResponse)(notWorthToMoveToTaskpool)
    case MaybeNewReachableSubtree(receiver, attachBelow, spanningSubTreeRoot) =>
      localTurnInstance(receiver, endpoint).maybeNewReachableSubtree(localTurnInstance(attachBelow, endpoint), spanningSubTreeRoot.map(localTurnInstance(_, endpoint))).map(_ => UnitResponse)(notWorthToMoveToTaskpool)
    case NewSuccessor(receiver, successor) =>
      localTurnInstance(receiver, endpoint).newSuccessor(localTurnInstance(successor, endpoint)).map(_ => UnitResponse)(notWorthToMoveToTaskpool)

    case TurnSubsume(receiver, newParent) =>
      localTurnInstance(receiver, endpoint).subsume(localLockInstance(newParent, endpoint)).map(_ => UnitResponse)(notWorthToMoveToTaskpool)
    case TurnUnlock(receiver) =>
      localTurnInstance(receiver, endpoint).unlock().map{ newParent =>
        assert(newParent.host == host.lockHost)
        LockResponse(newParent.guid)
      }(notWorthToMoveToTaskpool)
    case TurnGetLockedRoot(receiver) =>
      localTurnInstance(receiver, endpoint).getLockedRoot.map { res =>
        MaybeLockResponse(res)
      }(notWorthToMoveToTaskpool)
    case TurnTryLock(receiver) =>
      localTurnInstance(receiver, endpoint).tryLock().map { case TryLockResult(success, newParent) =>
        assert(newParent.host == host)
        TryLockResponse(success, newParent.guid)
      }(notWorthToMoveToTaskpool)
    case TurnLock(receiver) =>
      localTurnInstance(receiver, endpoint).lock().map { newParent =>
        assert(newParent.host == host)
        LockResponse(newParent.guid)
      }(notWorthToMoveToTaskpool)
    case TurnSpinOnce(receiver, backoff) =>
      localTurnInstance(receiver, endpoint).spinOnce(backoff).map { newParent =>
        assert(newParent.host == host)
        LockResponse(newParent.guid)
      }(notWorthToMoveToTaskpool)
    case TurnTrySubsume(receiver, lockedNewParent) =>
      localTurnInstance(receiver, endpoint).trySubsume(localLockInstance(lockedNewParent, endpoint)).map { res =>
        MaybeLockResponse(res.map { resNewParent =>
          assert(resNewParent.host == host)
          resNewParent.guid
        })
      }(notWorthToMoveToTaskpool)


    case NewPredecessors(receiver, predecessors) =>
      localTurnReflection(receiver, endpoint).newPredecessors(predecessors).map(_ => UnitResponse)(notWorthToMoveToTaskpool)
    case NewPhase(receiver, phase) =>
      localTurnReflection(receiver, endpoint).newPhase(phase).map(_ => UnitResponse)(notWorthToMoveToTaskpool)


    case LockSubsume(receiver, newParent) =>
      localLockInstance(receiver, endpoint).subsume(localLockInstance(newParent, endpoint)).map(_ => UnitResponse)(notWorthToMoveToTaskpool)
    case LockUnlock(receiver) =>
      localLockInstance(receiver, endpoint).unlock().map{ newParent =>
        assert(newParent.host == host.lockHost)
        LockResponse(newParent.guid)
      }(notWorthToMoveToTaskpool)
    case LockGetLockedRoot(receiver) =>
      localLockInstance(receiver, endpoint).getLockedRoot.map { res =>
        MaybeLockResponse(res)
      }(notWorthToMoveToTaskpool)
    case LockTryLock(receiver) =>
      localLockInstance(receiver, endpoint).tryLock().map { case TryLockResult(success, newParent) =>
        assert(newParent.host == host)
        TryLockResponse(success, newParent.guid)
      }(notWorthToMoveToTaskpool)
    case LockLock(receiver) =>
      localLockInstance(receiver, endpoint).lock().map { newParent =>
        assert(newParent.host == host)
        LockResponse(newParent.guid)
      }(notWorthToMoveToTaskpool)
    case LockSpinOnce(receiver, backoff) =>
      localLockInstance(receiver, endpoint).spinOnce(backoff).map { newParent =>
        assert(newParent.host == host)
        LockResponse(newParent.guid)
      }(notWorthToMoveToTaskpool)
    case LockTrySubsume(receiver, lockedNewParent) =>
      localLockInstance(receiver, endpoint).trySubsume(localLockInstance(lockedNewParent, endpoint)).map { res =>
        MaybeLockResponse(res.map { resNewParent =>
          assert(resNewParent.host == host)
          resNewParent.guid
        })
      }(notWorthToMoveToTaskpool)

    case otherwise =>
      throw new AssertionError("undefined message : "+otherwise)
  }

  class FullMVTurnMirrorProxyToEndpoint(val guid: Host.GUID, endpoint: EndPointWithInfrastructure[Msg]) extends FullMVTurnProxy {
    override def addRemoteBranch(forPhase: Type): Future[Unit] = {
      doRequest(endpoint, AddRemoteBranch(guid, forPhase)).map(_ => ())(notWorthToMoveToTaskpool)
    }

    override def asyncRemoteBranchComplete(forPhase: TurnPhase.Type): Unit = {
      doAsync(endpoint, AsyncRemoteBranchComplete(guid, forPhase))
    }
    override def acquirePhaseLockAndGetEstablishmentBundle(): Future[(TurnPhase.Type, TransactionSpanningTreeNode[FullMVTurn])] = {
      doRequest(endpoint, AcquirePhaseLockAndGetEstablishmentBundle(guid)).map {
        case AcquirePhaseLockAndGetEstablishmentBundleResponse(phase, tree) =>
          (phase, tree.map(localTurnInstance(_, endpoint)))
      }(executeInTaskPool)
    }
    override def addPredecessorAndReleasePhaseLock(predecessorSpanningTree: TransactionSpanningTreeNode[FullMVTurn]): Future[Unit] = {
      doRequest(endpoint, AddPredecessorAndReleasePhaseLock(guid,  predecessorSpanningTree.map { turn =>
        assert(turn.host == host)
        turn.guid
      })).map(_ => ())(notWorthToMoveToTaskpool)
    }
    override def asyncReleasePhaseLock(): Unit = {
      doAsync(endpoint, AsyncReleasePhaseLock(guid))
    }
    override def maybeNewReachableSubtree(attachBelow: FullMVTurn, spanningSubTreeRoot: TransactionSpanningTreeNode[FullMVTurn]): Future[Unit] = {
      assert(attachBelow.host == host)
      doRequest(endpoint, MaybeNewReachableSubtree(guid, attachBelow.guid, spanningSubTreeRoot.map { turn =>
        assert(turn.host == host)
        turn.guid
      })).map(_ => ())(notWorthToMoveToTaskpool)
    }
    override def newSuccessor(successor: FullMVTurn): Future[Unit] = {
      assert(successor.host == host)
      doRequest(endpoint, NewSuccessor(guid, successor.guid)).map(_ => ())(notWorthToMoveToTaskpool)
    }

    override def subsume(lockedNewParent: SubsumableLock): Future[Unit] = {
      assert(lockedNewParent.host == host.lockHost)
      doRequest(endpoint, TurnSubsume(guid, lockedNewParent.guid)).map(_ => ())(notWorthToMoveToTaskpool)
    }
    override def unlock(): Future[SubsumableLock] = {
      doRequest(endpoint, TurnUnlock(guid)).map {
        case LockResponse(newParent) =>
          localLockInstance(newParent, endpoint)
      }(executeInTaskPool)
    }
    override def getLockedRoot: Future[Option[Host.GUID]] = {
      doRequest(endpoint, TurnGetLockedRoot(guid)).map {
        case MaybeLockResponse(maybeRoot) =>
          maybeRoot
      }(notWorthToMoveToTaskpool)
    }
    override def tryLock(): Future[TryLockResult] = {
      doRequest(endpoint, TurnTryLock(guid)).map {
        case TryLockResponse(success, newParent) =>
          TryLockResult(success, localLockInstance(newParent, endpoint))
      }(executeInTaskPool)
    }
    override def lock(): Future[SubsumableLock] = {
      doRequest(endpoint, TurnLock(guid)).map {
        case LockResponse(newParent) =>
          localLockInstance(newParent, endpoint)
      }(executeInTaskPool)
    }
    override def spinOnce(backoff: Host.GUID): Future[SubsumableLock] = {
      doRequest(endpoint, TurnSpinOnce(guid, backoff)).map {
        case LockResponse(newParent) =>
          localLockInstance(newParent, endpoint)
      }(executeInTaskPool)
    }
    override def trySubsume(lockedNewParent: SubsumableLock): Future[Option[SubsumableLock]] = {
      assert(lockedNewParent.host == host.lockHost)
      doRequest(endpoint, TurnTrySubsume(guid, lockedNewParent.guid)).map {
        case MaybeLockResponse(newParentIfFailed) =>
          newParentIfFailed.map(localLockInstance(_, endpoint))
      }(executeInTaskPool)
    }
  }

  class FullMVTurnReflectionProxyToEndpoint(val guid: Host.GUID, endpoint: EndPointWithInfrastructure[Msg]) extends FullMVTurnReflectionProxy {
    override def newPredecessors(predecessors: Seq[GUID]): Future[Unit] = {
      doRequest(endpoint, NewPredecessors(guid, predecessors)).map(_ => ())(notWorthToMoveToTaskpool)
    }

    override def newPhase(phase: TurnPhase.Type): Future[Unit] = {
      doRequest(endpoint, NewPhase(guid, phase)).map(_ => ())(notWorthToMoveToTaskpool)
    }
  }

  class SubsumableLockMirrorProxyToEndpoint(val guid: Host.GUID, endpoint: EndPointWithInfrastructure[Msg]) extends SubsumableLockProxy {
    override def subsume(lockedNewParent: SubsumableLock): Future[Unit] = {
      assert(lockedNewParent.host == host.lockHost)
      doRequest(endpoint, LockSubsume(guid, lockedNewParent.guid)).map(_ => ())(notWorthToMoveToTaskpool)
    }
    override def unlock(): Future[SubsumableLock] = {
      doRequest(endpoint, LockUnlock(guid)).map {
        case LockResponse(newParent) =>
          localLockInstance(newParent, endpoint)
      }(executeInTaskPool)
    }
    override def getLockedRoot: Future[Option[Host.GUID]] = {
      doRequest(endpoint, LockGetLockedRoot(guid)).map {
        case MaybeLockResponse(maybeRoot) =>
          maybeRoot
      }(notWorthToMoveToTaskpool)
    }
    override def tryLock(): Future[TryLockResult] = {
      doRequest(endpoint, LockTryLock(guid)).map {
        case TryLockResponse(success, newParent) =>
          TryLockResult(success, localLockInstance(newParent, endpoint))
      }(executeInTaskPool)
    }
    override def lock(): Future[SubsumableLock] = {
      doRequest(endpoint, LockLock(guid)).map {
        case LockResponse(newParent) =>
          localLockInstance(newParent, endpoint)
      }(executeInTaskPool)
    }
    override def spinOnce(backoff: Host.GUID): Future[SubsumableLock] = {
      doRequest(endpoint, LockSpinOnce(guid, backoff)).map {
        case LockResponse(newParent) =>
          localLockInstance(newParent, endpoint)
      }(executeInTaskPool)
    }
    override def trySubsume(lockedNewParent: SubsumableLock): Future[Option[SubsumableLock]] = {
      assert(lockedNewParent.host == host.lockHost)
      doRequest(endpoint, LockTrySubsume(guid, lockedNewParent.guid)).map {
        case MaybeLockResponse(newParentIfFailed) =>
          newParentIfFailed.map(localLockInstance(_, endpoint))
      }(executeInTaskPool)
    }
  }

  class ReactiveReflectionProxyToEndpoint(endpoint: EndPointWithInfrastructure[Msg]) extends ReactiveReflectionProxy[Pulse[P]] {
    override def asyncIncrementFrame(turn: FullMVTurn): Unit = {
      assert(turn.host == host)
      doAsync(endpoint, AsyncIncrementFrame(turn.guid))
    }
    override def asyncIncrementSupersedeFrame(turn: FullMVTurn, supersede: FullMVTurn): Unit = {
      assert(turn.host == host)
      assert(supersede.host == host)
      doAsync(endpoint, AsyncIncrementSupersedeFrame(turn.guid, supersede.guid))
    }
    override def asyncResolvedUnchanged(turn: FullMVTurn): Unit = {
      assert(turn.host == host)
      doAsync(endpoint, AsyncResolveUnchanged(turn.guid))
    }
    override def asyncResolvedUnchangedFollowFrame(turn: FullMVTurn, followFrame: FullMVTurn): Unit = {
      assert(turn.host == host)
      assert(followFrame.host == host)
      doAsync(endpoint, AsyncResolveUnchangedFollowFrame(turn.guid, followFrame.guid))
    }
    override def asyncNewValue(turn: FullMVTurn, value: Pulse[P]): Unit = {
      assert(turn.host == host)
      doAsync(endpoint, AsyncNewValue(turn.guid, Pluse.fromPulse(value)))
    }
    override def asyncNewValueFollowFrame(turn: FullMVTurn, value: Pulse[P], followFrame: FullMVTurn): Unit = {
      assert(turn.host == host)
      assert(followFrame.host == host)
      doAsync(endpoint, AsyncNewValueFollowFrame(turn.guid, Pluse.fromPulse(value), followFrame.guid))
    }
  }
}
