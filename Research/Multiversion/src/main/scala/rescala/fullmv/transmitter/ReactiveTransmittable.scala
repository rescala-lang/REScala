package rescala.fullmv.transmitter

import java.io.{ByteArrayInputStream, ByteArrayOutputStream, ObjectInputStream, ObjectOutputStream}
import java.util.concurrent.{ConcurrentHashMap, Executor, ThreadLocalRandom}

import rescala.core._
import rescala.fullmv.TurnPhase.Type
import rescala.fullmv.mirrors.Host.GUID
import rescala.fullmv.mirrors._
import rescala.fullmv.sgt.synchronization.SubsumableLock
import rescala.fullmv.sgt.synchronization.SubsumableLock.TryLockResult
import rescala.fullmv._
import rescala.reactives.Signal
import retier.transmitter._

import scala.annotation.tailrec
import scala.concurrent._
import scala.util.{Failure, Success}

sealed trait MessageType
case object AsyncRequest extends MessageType
case class Request(requestId: Long) extends MessageType
case class Response(requestId: Long) extends MessageType

object ReactiveTransmittable {
  val DEBUG = FullMVEngine.DEBUG

  val notWorthToMoveToTaskpool: ExecutionContextExecutor = ExecutionContext.fromExecutor(new Executor{
    override def execute(command: Runnable) = command.run()
  })

  type EndPointWithInfrastructure[T] = Endpoint[MessageWithInfrastructure[T], MessageWithInfrastructure[T]]
  type MessageWithInfrastructure[T] = (Long, T)

  val ASYNC_REQUEST = 0

  sealed trait Message[+P]
  sealed trait Async[+P] extends Message[P]
  sealed trait Request[+P] extends Message[P] {
    type Response <: ReactiveTransmittable.this.Response[P]
  }
  sealed trait Response[+P] extends Message[P]
  case object UnitResponse extends Response[Nothing]

  case class Connect[P](turn: Host.GUID) extends Request[P]{ override type Response = Initialize[P] }
  case class Initialize[P](initValues: Seq[(Host.GUID, P)], mabyeFirstFrame: Option[Host.GUID]) extends Response[P]

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
        case Pulse.Exceptional(e) => {
          val baos = new ByteArrayOutputStream()
          val oos = new ObjectOutputStream(baos)
          oos.writeObject(e)
          oos.flush()
          Pluse.Exceptional(baos.toByteArray)
        }
      }
    }
    case object NoChange extends Pluse[Nothing]
    final case class Value[+P](update: P) extends Pluse[P]
    final case class Exceptional(serializedThrowable: Array[Byte]) extends Pluse[Nothing]
  }

  implicit def signalTransmittable[P, S](implicit host: FullMVEngine, messageTransmittable: Transmittable[MessageWithInfrastructure[Message[Pluse[P]]], S, MessageWithInfrastructure[Message[Pluse[P]]]], serializable: Serializable[S]): Transmittable[Signal[P, FullMVStruct], S, Signal[P, FullMVStruct]] = new PushBasedTransmittable[Signal[P, FullMVStruct], MessageWithInfrastructure[Message[Pluse[P]]], S, MessageWithInfrastructure[Message[Pluse[P]]], Signal[P, FullMVStruct]] {
    type Message = ReactiveTransmittable.this.Message[Pluse[P]]
    type Async = ReactiveTransmittable.this.Async[Pluse[P]]
    type Request = ReactiveTransmittable.this.Request[Pluse[P]]
    type Response = ReactiveTransmittable.this.Response[Pluse[P]]

    val executeInTaskPool: ExecutionContext = ExecutionContext.fromExecutorService(host.threadPool)

    val requestTracker = new ConcurrentHashMap[Long, Promise[_ <: Response]]()

    def handleResponse(requestId: Long, response: Response): Unit = {
      val promise = requestTracker.remove(requestId).asInstanceOf[Promise[Response]] /* typesafety yay */
      assert(promise != null, s"request $requestId unknown!")
      promise.complete(Success(response))
    }

    def handleMessage(localReactive: Either[ReadableReactive[Pulse[P], FullMVStruct], ReactiveReflection[Pulse[P]]], endpoint: EndPointWithInfrastructure[Message])(msg: MessageWithInfrastructure[Message]): Unit = {
      if(ReactiveTransmittable.DEBUG) println(s"[${Thread.currentThread().getName}] $host receive incoming $msg")
      msg match {
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
                  endpoint.send((requestId, response))
              }(ReactiveTransmittable.notWorthToMoveToTaskpool)
            }
          })
        case (requestId, response: Response) =>
          if(ReactiveTransmittable.DEBUG) println(s"[${Thread.currentThread().getName}] $host processing response $response")
          handleResponse(requestId, response)
      }
    }

    override def send(value: Signal[P, FullMVStruct], remote: RemoteRef, endpoint: EndPointWithInfrastructure[Message]): MessageWithInfrastructure[Message] = {
      endpoint.receive.notify (handleMessage(Left(value: ReadableReactive[Pulse[P], FullMVStruct]), endpoint))
      (0L, UnitResponse)
    }

    def doAsync(endpoint: EndPointWithInfrastructure[Message], parameters: Async): Unit = {
      if(ReactiveTransmittable.DEBUG) println(s"[${Thread.currentThread().getName}] $host send async $parameters")
      endpoint.send((0L, parameters))
    }

    def doRequest(endpoint: EndPointWithInfrastructure[Message],  parameters: Request): Future[parameters.Response] = {
      val promise = Promise[parameters.Response]()
      @inline @tailrec def createRequest(): Long = {
        val requestId = ThreadLocalRandom.current().nextLong()
        if(requestId != 0 && requestTracker.putIfAbsent(requestId, promise) == null) requestId else createRequest()
      }
      val requestId = createRequest()
      if(ReactiveTransmittable.DEBUG) println(s"[${Thread.currentThread().getName}] $host send request $requestId: $parameters")
      endpoint.send((requestId, parameters))
      promise.future
    }

    def localLockInstance(guid: Host.GUID, endpoint: EndPointWithInfrastructure[Message]): SubsumableLock = {
      host.lockHost.getCachedOrReceiveRemote(guid) { doCache =>
        if(ReactiveTransmittable.DEBUG) println(s"[${Thread.currentThread().getName}] $host connecting lock $guid")
        val instance = new SubsumableLockReflection(host.lockHost, guid, new SubsumableLockMirrorProxyToEndpoint(guid, endpoint))
        doCache(instance)
        instance
      }
    }

    def localTurnInstance0(guid: Host.GUID, endpoint: EndPointWithInfrastructure[Message]): FullMVTurn = {
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
    def localTurnInstance(guid: Host.GUID, endpoint: EndPointWithInfrastructure[Message]): FullMVTurn = {
      val turn = localTurnInstance0(guid, endpoint)
      // TODO there might be a cleverer way to do this..
      turn.awaitPhase(TurnPhase.Initialized + 1)
      turn
    }
    def localTurnReflection(guid: Host.GUID, endpoint: EndPointWithInfrastructure[Message]): FullMVTurnReflection = {
      localTurnInstance0(guid, endpoint).asInstanceOf[FullMVTurnReflection]
    }

    override def receive(value: MessageWithInfrastructure[Message], remote: RemoteRef, endpoint: EndPointWithInfrastructure[Message]): Signal[P, FullMVStruct] = {
      val valuePersistency = ValuePersistency.DerivedSignal[P]
      val turn = host.newTurn()
      turn.awaitAndSwitchPhase(TurnPhase.Executing)
      val state = turn.makeStructState(valuePersistency)
      val reflection = new ReactiveReflectionImpl[P](host, None, state, "SignalReflection") with Signal[P, FullMVStruct] {
        override def disconnect()(implicit engine: Engine[FullMVStruct]): Unit = ???
      }
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

    def handleAsync(localReactive: Either[ReadableReactive[Pulse[P], FullMVStruct], ReactiveReflection[Pulse[P]]], endpoint: EndPointWithInfrastructure[Message], message: Async): Unit = message match {
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

    def handleRequest(localReactive: Either[ReadableReactive[Pulse[P], FullMVStruct], ReactiveReflection[Pulse[P]]], endpoint: EndPointWithInfrastructure[Message], request: Request): Future[Response] = request match {
      case Connect(turn) =>
        val (initValues, maybeFirstFrame) = ReactiveMirror[Pulse[P]](localReactive.left.get, localTurnInstance(turn, endpoint), new ReactiveReflectionProxyToEndpoint(endpoint), reflectionIsTransient = false, "SignalMirror")
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

    class FullMVTurnMirrorProxyToEndpoint(val guid: Host.GUID, endpoint: EndPointWithInfrastructure[Message]) extends FullMVTurnProxy {
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

    class FullMVTurnReflectionProxyToEndpoint(val guid: Host.GUID, endpoint: EndPointWithInfrastructure[Message]) extends FullMVTurnReflectionProxy {
      override def newPredecessors(predecessors: Seq[GUID]): Future[Unit] = {
        doRequest(endpoint, NewPredecessors(guid, predecessors)).map(_ => ())(notWorthToMoveToTaskpool)
      }

      override def newPhase(phase: TurnPhase.Type): Future[Unit] = {
        doRequest(endpoint, NewPhase(guid, phase)).map(_ => ())(notWorthToMoveToTaskpool)
      }
    }

    class SubsumableLockMirrorProxyToEndpoint(val guid: Host.GUID, endpoint: EndPointWithInfrastructure[Message]) extends SubsumableLockProxy {
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

    class ReactiveReflectionProxyToEndpoint(endpoint: EndPointWithInfrastructure[Message]) extends ReactiveReflectionProxy[Pulse[P]] {
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
}
