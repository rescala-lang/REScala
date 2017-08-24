package rescala.fullmv.transmitter

import java.io.{ByteArrayInputStream, ByteArrayOutputStream, ObjectInputStream, ObjectOutputStream}
import java.util.concurrent.{ConcurrentHashMap, ThreadLocalRandom}

import rescala.core._
import rescala.fullmv.mirrors.Host.GUID
import rescala.fullmv.mirrors._
import rescala.fullmv.sgt.synchronization.SubsumableLock
import rescala.fullmv.sgt.synchronization.SubsumableLock.TryLockResult
import rescala.fullmv._
import rescala.reactives.Signal
import retier.transmitter._

import scala.annotation.tailrec
import scala.concurrent.duration.Duration
import scala.concurrent._
import scala.util.{Failure, Success}

sealed trait MessageType
case object AsyncRequest extends MessageType
case class Request(requestId: Long) extends MessageType
case class Response(requestId: Long) extends MessageType

object ReactiveTransmittable {
  val DEBUG = false

  val notWorthToMoveToTaskpool: ExecutionContextExecutor = ExecutionContext.fromExecutor(_.run())

  type EndPointWithInfrastructure[T] = Endpoint[MessageWithInfrastructure[T], MessageWithInfrastructure[T]]
  type MessageWithInfrastructure[T] = (Int, Long, T)

  val ASYNC_REQUEST = 0
  val RESPONSE = 0

  // Reactive Mirror to Reflection: (Turn, Value, Turn) => Unit: Ref, Value, Ref
  val CONNECT = 1
//  def asyncIncrementFrame(turn: FullMVTurn): Unit
  val ASYNC_INCREMENT_FRAME = 2
//  def asyncIncrementSupersedeFrame(turn: FullMVTurn, supersede: FullMVTurn): Unit
  val ASYNC_INCREMENT_SUPERSEDE_FRAME = 3
//  def asyncResolvedUnchanged(turn: FullMVTurn): Unit
  val ASYNC_RESOLVE_UNCHANGED = 4
//  def asyncResolvedUnchangedFollowFrame(turn: FullMVTurn, followFrame: FullMVTurn): Unit
  val ASYNC_RESOLVE_UNCHNAGED_FOLLOW_FRAME = 5
//  def asyncNewValue(turn: FullMVTurn, value: P): Unit
  val ASYNC_NEW_VALUE = 6
//  def asyncNewValueFollowFrame(turn: FullMVTurn, value: P, followFrame: FullMVTurn): Unit
  val ASYNC_NEW_VALUE_FOLLOW_FRAME = 7

  // Turn Reflection to Mirror: (Phase, Tree, Turn) => (Phase, Tree): Phase, Tree, Ref
  //  def addReplicator(replicator: FullMVTurnReflectionProxy): (TurnPhase.Type, Set[Host.GUID])
  val ADD_REPLICATOR = 10
//  def asyncRemoteBranchComplete(forPhase: TurnPhase.Type): Unit
  val ASYNC_REMOTE_BRANCH_COMPLETE = 11
//  def acquirePhaseLockAndGetEstablishmentBundle(): Future[(TurnPhase.Type, TransactionSpanningTreeNode[FullMVTurn])]
  val ACQUIRE_PHASE_LOCK_AND_GET_ESTABLISHMENT_BUNDLE = 12
//  def blockingAddPredecessorAndReleasePhaseLock(predecessorSpanningTree: TransactionSpanningTreeNode[FullMVTurn]): Unit
  val BLOCKING_ADD_PREDECESSOR_AND_RELEASE_PHASE_LOCK = 13
//  def asyncReleasePhaseLock(): Unit
  val ASYNC_RELEASE_PHASE_LOCK = 14
//  def maybeNewReachableSubtree(attachBelow: FullMVTurn, spanningSubTreeRoot: TransactionSpanningTreeNode[FullMVTurn]): Future[Unit]
  val MAYBE_NEW_REACHABLE_SUBTREE = 15
//  def newSuccessor(successor: FullMVTurn): Future[Unit]
  val NEW_SUCCESSOR = 16

//  def getLockedRoot: Option[Host.GUID]
  val TURN_GET_LOCKED_ROOT = 41
//  def tryLock(): SubsumableLock.TryLockResult
  val TURN_TRY_LOCK = 42
//  def lock(): SubsumableLock
  val TURN_LOCK = 43
//  def spinOnce(backoff: Long): SubsumableLock
  val TURN_SPIN_ONCE = 44
//  def trySubsume(lockedNewParent: SubsumableLock): Option[SubsumableLock]
  val TURN_TRY_SUBSUME = 45
//  def subsume(lockedNewParent: SubsumableLock): Unit
  val TURN_SUBSUME = 46
//  def unlock(): SubsumableLock
  val TURN_ASYNC_UNLOCK = 47


  // Turn Mirror To Reflection: (Phase, Iterable[Turn]) => Unit: Phase, Iterable[Ref]
//  def newPredecessors(predecessors: Iterable[Host.GUID]): Future[Unit]
  val NEW_PREDECESSORS = 21
//  def newPhase(phase: TurnPhase.Type): Future[Unit]
  val NEW_PHASE = 22

  // SubsumableLock Reflection To Mirror: (Long, Lock) => (Boolean, Lock): Long, Boolean, Ref
//  def getLockedRoot: Option[Host.GUID]
  val LOCK_GET_LOCKED_ROOT = 31
//  def tryLock(): SubsumableLock.TryLockResult
  val LOCK_TRY_LOCK = 32
//  def lock(): SubsumableLock
  val LOCK_LOCK = 33
//  def spinOnce(backoff: Long): SubsumableLock
  val LOCK_SPIN_ONCE = 34
//  def trySubsume(lockedNewParent: SubsumableLock): Option[SubsumableLock]
  val LOCK_TRY_SUBSUME = 35
//  def subsume(lockedNewParent: SubsumableLock): Unit
  val LOCK_SUBSUME = 36
//  def unlock(): SubsumableLock
  val LOCK_ASYNC_UNLOCK = 37

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

  type ParametersOrReturns[P] = (Seq[(Host.GUID, Pluse[P])], Host.GUID, Pluse[P], Host.GUID, TurnPhase.Type, CaseClassTransactionSpanningTreeNode[Host.GUID], Seq[Host.GUID], Long, Boolean)

  implicit def signalTransmittable[P, S](implicit host: FullMVEngine, messageTransmittable: Transmittable[MessageWithInfrastructure[ParametersOrReturns[P]], S, MessageWithInfrastructure[ParametersOrReturns[P]]], serializable: Serializable[S]): Transmittable[Signal[P, FullMVStruct], S, Signal[P, FullMVStruct]] = new PushBasedTransmittable[Signal[P, FullMVStruct], MessageWithInfrastructure[ParametersOrReturns[P]], S, MessageWithInfrastructure[ParametersOrReturns[P]], Signal[P, FullMVStruct]] {
    type ParametersOrReturns = ReactiveTransmittable.this.ParametersOrReturns[P]
    val allEmpty: ParametersOrReturns = (null, Host.dummyGuid, null.asInstanceOf[Pluse[P]], Host.dummyGuid, TurnPhase.dummy, null, null, 0L, false)

//    val executeInTaskPool: ExecutionContext = ExecutionContext.fromExecutorService(host.threadPool)

    val requestTracker = new ConcurrentHashMap[Long, Promise[ParametersOrReturns]]()

    def handleResponse(requestId: Long, returns: ParametersOrReturns): Unit = {
      val promise = requestTracker.remove(requestId)
      assert(promise != null, s"request $requestId unknown!")
      promise.complete(Success(returns))
    }

    def handleMessage(localReactive: Either[ReadableReactive[Pulse[P], FullMVStruct], ReactiveReflectionProxy[Pulse[P]]], endpoint: EndPointWithInfrastructure[ParametersOrReturns])(msg: MessageWithInfrastructure[ParametersOrReturns]): Unit = msg match {
      case (RESPONSE, ASYNC_REQUEST, _) =>
        throw new AssertionError("repsonses to async requests shouldn't exist..")
      case (RESPONSE, requestId, returns) =>
        handleResponse(requestId, returns)
      case (op, ASYNC_REQUEST, parameters) =>
        handleRequest(localReactive, endpoint, op, parameters)
      case (op, requestId, parameters) =>
        handleRequest(localReactive, endpoint, op, parameters).onComplete {
          // TODO better exception propagation
          case Failure(e) => new Exception("Local Request Handling threw Exception", e).printStackTrace()
          case Success(v) => endpoint.send((RESPONSE, requestId, v))
        }(ReactiveTransmittable.notWorthToMoveToTaskpool)
    }

    override def send(value: Signal[P, FullMVStruct], remote: RemoteRef, endpoint: EndPointWithInfrastructure[ParametersOrReturns]): MessageWithInfrastructure[ParametersOrReturns] = {
      endpoint.receive.notify (handleMessage(Left(value: ReadableReactive[Pulse[P], FullMVStruct]), endpoint))
      (0, 0L, allEmpty)
    }

    def doBlocking(endpoint: EndPointWithInfrastructure[ParametersOrReturns], op: Int, parameters: ParametersOrReturns): ParametersOrReturns = {
      Await.result(doRequest(endpoint, op, parameters), Duration.Inf)
    }
    def doAsync(endpoint: EndPointWithInfrastructure[ParametersOrReturns], op: Int, parameters: ParametersOrReturns): Unit = {
      // TODO at some point actually use async? Currently sync for exception returns
      doBlocking(endpoint, op, parameters)
    }

    def doRequest(endpoint: EndPointWithInfrastructure[ParametersOrReturns], op: Int, parameters: ParametersOrReturns): Future[ParametersOrReturns] = {
      val promise = Promise[ParametersOrReturns]()
      @inline @tailrec def createRequest(): Long = {
        val requestId = ThreadLocalRandom.current().nextLong()
        if(requestId != 0 && requestTracker.putIfAbsent(requestId, promise) == null) requestId else createRequest()
      }
      val requestId = createRequest()
      if(DEBUG) println(s"[${Thread.currentThread().getName}] send request $requestId: ($op, $parameters)")
      endpoint.send((op, requestId, parameters))
      promise.future
    }

    def localLockInstance(guid: Host.GUID, endpoint: EndPointWithInfrastructure[ParametersOrReturns]): SubsumableLock = {
      host.lockHost.getCachedOrReceiveRemote(guid) { doCache =>
        val instance = new SubsumableLockReflection(host.lockHost, guid, new SubsumableLockMirrorProxyToEndpoint(guid, endpoint))
        doCache(instance)
        instance
      }
    }

    def localTurnInstance(guid: Host.GUID, endpoint: EndPointWithInfrastructure[ParametersOrReturns]): FullMVTurn = {
      host.getCachedOrReceiveRemote(guid) { doCache =>
        val instance = new FullMVTurnReflection(host, guid, new FullMVTurnMirrorProxyToEndpoint(guid, endpoint), host.timeout)
        doCache(instance)
        val(_, _, _, _, initPhase, _, initPreds, _, _) = doBlocking(endpoint, ADD_REPLICATOR, allEmpty.copy(_2 = guid))
        instance.newPredecessors(initPreds)
        instance.newPhase(initPhase)
        instance
      }
    }
    def localTurnReflection(guid: Host.GUID, endpoint: EndPointWithInfrastructure[ParametersOrReturns]): FullMVTurnReflection = {
      // TODO track these separately?
      localTurnInstance(guid, endpoint).asInstanceOf[FullMVTurnReflection]
    }

    override def receive(value: MessageWithInfrastructure[ParametersOrReturns], remote: RemoteRef, endpoint: EndPointWithInfrastructure[ParametersOrReturns]): Signal[P, FullMVStruct] = {
      val valuePersistency = ValuePersistency.DerivedSignal[P]
      val turn = host.newTurn()
      turn.awaitAndSwitchPhase(TurnPhase.Executing)
      val reflection = turn.create(Set(), valuePersistency) { initialState =>
        val reflection = new ReactiveReflectionImpl[P](host, None, initialState, "SignalReflection") with Signal[P, FullMVStruct] {
          override def disconnect()(implicit engine: Engine[FullMVStruct]): Unit = ???
        }
        endpoint.receive.notify (handleMessage(Right(reflection: ReactiveReflectionProxy[Pulse[P]]), endpoint))
        val (initValues, maybeFirstFrame, _, _, _, _, _, _, maybe) = doBlocking(endpoint, CONNECT, allEmpty.copy(_2 = turn.guid))

        val reflectionInitValues = initValues.map{ case (mirrorTurn, v) => localTurnInstance(mirrorTurn, endpoint) -> v }
        val reflectionMaybeFirstFrame = if(maybe) Some(localTurnInstance(maybeFirstFrame, endpoint)) else None

        reflection.state.retrofitSinkFrames(reflectionInitValues.map(_._1), reflectionMaybeFirstFrame, +1)
        for((reflectionTurn, v) <- reflectionInitValues) reflection.buffer(reflectionTurn, v.toPulse)

        reflection
      }
      turn.awaitAndSwitchPhase(TurnPhase.Completed)

      reflection
    }

    def handleRequest(localReactive: Either[ReadableReactive[Pulse[P], FullMVStruct], ReactiveReflectionProxy[Pulse[P]]], endpoint: EndPointWithInfrastructure[ParametersOrReturns], op: Int, parameters: ParametersOrReturns): Future[ParametersOrReturns] = (op, parameters) match {
      case (CONNECT, (_, turn, _, _, _, _, _, _, _)) =>
        val (initValues, maybeFirstFrame) = ReactiveMirror[Pulse[P]](localReactive.left.get, localTurnInstance(turn, endpoint), new ReactiveReflectionProxyToEndpoint(endpoint), reflectionIsTransient = false, "SignalMirror")
        Future.successful(allEmpty.copy(_1 = initValues.map { case (aTurn, value) =>
          assert(aTurn.host == host)
          (aTurn.guid, Pluse.fromPulse(value))
        }, _2 = maybeFirstFrame match {
          case Some(aTurn) =>
            assert(aTurn.host == host)
            aTurn.guid
          case None => Host.dummyGuid
        }, _9 = maybeFirstFrame.isDefined))
      case (ASYNC_INCREMENT_FRAME, (_, turn, _, _, _, _, _, _, _)) =>
        localReactive.right.get.asyncIncrementFrame(localTurnInstance(turn, endpoint))
        Future.successful(allEmpty)
      case (ASYNC_INCREMENT_SUPERSEDE_FRAME, (_, turn, _, supersede, _, _, _, _, _)) =>
        localReactive.right.get.asyncIncrementSupersedeFrame(localTurnInstance(turn, endpoint), localTurnInstance(supersede, endpoint))
        Future.successful(allEmpty)
      case (ASYNC_RESOLVE_UNCHANGED, (_, turn, _, _, _, _, _, _, _)) =>
        localReactive.right.get.asyncResolvedUnchanged(localTurnInstance(turn, endpoint))
        Future.successful(allEmpty)
      case (ASYNC_RESOLVE_UNCHNAGED_FOLLOW_FRAME, (_, turn, _, followFrame, _, _, _, _, _)) =>
        localReactive.right.get.asyncResolvedUnchangedFollowFrame(localTurnInstance(turn, endpoint), localTurnInstance(followFrame, endpoint))
        Future.successful(allEmpty)
      case (ASYNC_NEW_VALUE, (_, turn, value, _, _, _, _, _, _)) =>
        localReactive.right.get.asyncNewValue(localTurnInstance(turn, endpoint), value.toPulse)
        Future.successful(allEmpty)
      case (ASYNC_NEW_VALUE_FOLLOW_FRAME, (_, turn, value, followFrame, _, _, _, _, _)) =>
        localReactive.right.get.asyncNewValueFollowFrame(localTurnInstance(turn, endpoint), value.toPulse, localTurnInstance(followFrame, endpoint))
        Future.successful(allEmpty)


      case (ADD_REPLICATOR, (_, receiver, _, _, _, _, _, _, _)) =>
        val (initPhase, initPreds) = localTurnInstance(receiver, endpoint).addReplicator(new FullMVTurnReflectionProxyToEndpoint(receiver, endpoint))
        Future.successful(allEmpty.copy(_5 = initPhase, _7 = initPreds))
      case (ASYNC_REMOTE_BRANCH_COMPLETE, (_, receiver, _, _, forPhase, _, _, _, _)) =>
        localTurnInstance(receiver, endpoint).asyncRemoteBranchComplete(forPhase)
        Future.successful(allEmpty)
      case (ACQUIRE_PHASE_LOCK_AND_GET_ESTABLISHMENT_BUNDLE, (_, receiver, _, _, _, _, _, _, _)) =>
        localTurnInstance(receiver, endpoint).acquirePhaseLockAndGetEstablishmentBundle().map(res =>
        allEmpty.copy(_5 = res._1, _6 = res._2.map{ turn =>
          assert(turn.host == host)
          turn.guid
        }))(ReactiveTransmittable.notWorthToMoveToTaskpool)
      case (BLOCKING_ADD_PREDECESSOR_AND_RELEASE_PHASE_LOCK, (_, receiver, _, _, _, predecessorSpanningTree, _, _, _)) =>
        localTurnInstance(receiver, endpoint).addPredecessorAndReleasePhaseLock(predecessorSpanningTree.map(localTurnInstance(_, endpoint))).map(_ => allEmpty)(notWorthToMoveToTaskpool)
      case (ASYNC_RELEASE_PHASE_LOCK, (_, receiver, _, _, _, _, _, _, _)) =>
        localTurnInstance(receiver, endpoint).asyncReleasePhaseLock()
        Future.successful(allEmpty)
      case (MAYBE_NEW_REACHABLE_SUBTREE, (_, receiver, _, attachBelow, _, spanningSubTreeRoot, _, _, _)) =>
        localTurnInstance(receiver, endpoint).maybeNewReachableSubtree(localTurnInstance(attachBelow, endpoint), spanningSubTreeRoot.map(localTurnInstance(_, endpoint))).map(_ => allEmpty)(notWorthToMoveToTaskpool)
      case (NEW_SUCCESSOR, (_, receiver, _, successor, _, _, _, _, _)) =>
        localTurnInstance(receiver, endpoint).newSuccessor(localTurnInstance(successor, endpoint)).map(_ => allEmpty)(notWorthToMoveToTaskpool)

      case (TURN_SUBSUME, (_, receiver, _, newParent, _, _, _, _, _)) =>
        localTurnInstance(receiver, endpoint).subsume(localLockInstance(newParent, endpoint)).map(_ => allEmpty)(notWorthToMoveToTaskpool)
      case (TURN_ASYNC_UNLOCK, (_, receiver, _, _, _, _, _, _, _)) =>
        localTurnInstance(receiver, endpoint).unlock().map{ newParent =>
          assert(newParent.host == host.lockHost)
          allEmpty.copy(_2 = newParent.guid)
        }(notWorthToMoveToTaskpool)
      case (TURN_GET_LOCKED_ROOT, (_, receiver, _, _, _, _, _, _, _)) =>
        localTurnInstance(receiver, endpoint).getLockedRoot.map { res =>
          allEmpty.copy(_2 = res.getOrElse(Host.dummyGuid), _9 = res.isDefined)
        }(notWorthToMoveToTaskpool)
      case (TURN_TRY_LOCK, (_, receiver, _, _, _, _, _, _, _)) =>
        localTurnInstance(receiver, endpoint).tryLock().map { case TryLockResult(success, newParent) =>
          assert(newParent.host == host)
          allEmpty.copy(_2 = newParent.guid, _9 = success)
        }(notWorthToMoveToTaskpool)
      case (TURN_LOCK, (_, receiver, _, _, _, _, _, _, _)) =>
        localTurnInstance(receiver, endpoint).lock().map { newParent =>
          assert(newParent.host == host)
          allEmpty.copy(_2 = newParent.guid)
        }(notWorthToMoveToTaskpool)
      case (TURN_SPIN_ONCE, (_, receiver, _, _, _, _, _, backoff, _)) =>
        localTurnInstance(receiver, endpoint).spinOnce(backoff).map { newParent =>
          assert(newParent.host == host)
          allEmpty.copy(_2 = newParent.guid)
        }(notWorthToMoveToTaskpool)
      case (TURN_TRY_SUBSUME, (_, receiver, _, lockedNewParent, _, _, _, _, _)) =>
        localTurnInstance(receiver, endpoint).trySubsume(localLockInstance(lockedNewParent, endpoint)).map { res =>
          allEmpty.copy(_2 = res match {
            case Some(resNewParent) =>
              assert(resNewParent.host == host)
              resNewParent.guid
            case None => Host.dummyGuid
          }, _9 = res.isEmpty)
        }(notWorthToMoveToTaskpool)


      case (NEW_PREDECESSORS, (_, receiver, _, _, _, _, predecessors, _, _)) =>
        localTurnReflection(receiver, endpoint).newPredecessors(predecessors).map(_ => allEmpty)(notWorthToMoveToTaskpool)
      case (NEW_PHASE, (_, receiver, _, _, phase, _, _, _, _)) =>
        localTurnReflection(receiver, endpoint).newPhase(phase).map(_ => allEmpty)(notWorthToMoveToTaskpool)


      case (LOCK_SUBSUME, (_, receiver, _, newParent, _, _, _, _, _)) =>
        localLockInstance(receiver, endpoint).subsume(localLockInstance(newParent, endpoint)).map(_ => allEmpty)(notWorthToMoveToTaskpool)
      case (LOCK_ASYNC_UNLOCK, (_, receiver, _, _, _, _, _, _, _)) =>
        localLockInstance(receiver, endpoint).unlock().map{ newParent =>
          assert(newParent.host == host.lockHost)
          allEmpty.copy(_2 = newParent.guid)
        }(notWorthToMoveToTaskpool)
      case (LOCK_GET_LOCKED_ROOT, (_, receiver, _, _, _, _, _, _, _)) =>
        localLockInstance(receiver, endpoint).getLockedRoot.map { res =>
          allEmpty.copy(_2 = res.getOrElse(Host.dummyGuid), _9 = res.isDefined)
        }(notWorthToMoveToTaskpool)
      case (LOCK_TRY_LOCK, (_, receiver, _, _, _, _, _, _, _)) =>
        localLockInstance(receiver, endpoint).tryLock().map { case TryLockResult(success, newParent) =>
          assert(newParent.host == host)
          allEmpty.copy(_2 = newParent.guid, _9 = success)
        }(notWorthToMoveToTaskpool)
      case (LOCK_LOCK, (_, receiver, _, _, _, _, _, _, _)) =>
        localLockInstance(receiver, endpoint).lock().map { newParent =>
          assert(newParent.host == host)
          allEmpty.copy(_2 = newParent.guid)
        }(notWorthToMoveToTaskpool)
      case (LOCK_SPIN_ONCE, (_, receiver, _, _, _, _, _, backoff, _)) =>
        localLockInstance(receiver, endpoint).spinOnce(backoff).map { newParent =>
          assert(newParent.host == host)
          allEmpty.copy(_2 = newParent.guid)
        }(notWorthToMoveToTaskpool)
      case (LOCK_TRY_SUBSUME, (_, receiver, _, lockedNewParent, _, _, _, _, _)) =>
        localLockInstance(receiver, endpoint).trySubsume(localLockInstance(lockedNewParent, endpoint)).map { res =>
          allEmpty.copy(_2 = res match {
            case Some(resNewParent) =>
              assert(resNewParent.host == host)
              resNewParent.guid
            case None => Host.dummyGuid
          }, _9 = res.isEmpty)
        }(notWorthToMoveToTaskpool)

      case otherwise =>
        throw new AssertionError("undefined message : "+otherwise)
    }

    class FullMVTurnMirrorProxyToEndpoint(val guid: Host.GUID, endpoint: EndPointWithInfrastructure[ParametersOrReturns]) extends FullMVTurnProxy {
      val withReceiver: ParametersOrReturns = allEmpty.copy(_2 = guid)

      override def asyncRemoteBranchComplete(forPhase: TurnPhase.Type): Unit = {
        doAsync(endpoint, ASYNC_REMOTE_BRANCH_COMPLETE, withReceiver.copy(_5 = forPhase))
      }
      override def acquirePhaseLockAndGetEstablishmentBundle(): Future[(TurnPhase.Type, TransactionSpanningTreeNode[FullMVTurn])] = {
        doRequest(endpoint, ACQUIRE_PHASE_LOCK_AND_GET_ESTABLISHMENT_BUNDLE, withReceiver).map {
          case (_, _, _, _, phase, tree, _, _, _) =>
            (phase, tree.map(localTurnInstance(_, endpoint)))
        }(notWorthToMoveToTaskpool)
      }
      override def addPredecessorAndReleasePhaseLock(predecessorSpanningTree: TransactionSpanningTreeNode[FullMVTurn]): Future[Unit] = {
        doRequest(endpoint, BLOCKING_ADD_PREDECESSOR_AND_RELEASE_PHASE_LOCK, withReceiver.copy(_6 = predecessorSpanningTree.map { turn =>
          assert(turn.host == host)
          turn.guid
        })).map(_ => ())(notWorthToMoveToTaskpool)
      }
      override def asyncReleasePhaseLock(): Unit = {
        doAsync(endpoint, ASYNC_RELEASE_PHASE_LOCK, withReceiver)
      }
      override def maybeNewReachableSubtree(attachBelow: FullMVTurn, spanningSubTreeRoot: TransactionSpanningTreeNode[FullMVTurn]): Future[Unit] = {
        assert(attachBelow.host == host)
        doRequest(endpoint, MAYBE_NEW_REACHABLE_SUBTREE, withReceiver.copy(_4 = attachBelow.guid, _6 = spanningSubTreeRoot.map { turn =>
          assert(turn.host == host)
          turn.guid
        })).map(_ => ())(notWorthToMoveToTaskpool)
      }
      override def newSuccessor(successor: FullMVTurn): Future[Unit] = {
        assert(successor.host == host)
        doRequest(endpoint, NEW_SUCCESSOR, withReceiver.copy(_4 = successor.guid)).map(_ => ())(notWorthToMoveToTaskpool)
      }

      override def subsume(lockedNewParent: SubsumableLock): Future[Unit] = {
        assert(lockedNewParent.host == host.lockHost)
        doRequest(endpoint, TURN_SUBSUME, withReceiver.copy(_4 = lockedNewParent.guid)).map(_ => ())(notWorthToMoveToTaskpool)
      }
      override def unlock(): Future[SubsumableLock] = {
        doRequest(endpoint, TURN_ASYNC_UNLOCK, withReceiver).map {
          case (_, newParent, _, _, _, _, _, _, _) =>
            localLockInstance(newParent, endpoint)
        }(notWorthToMoveToTaskpool)
      }
      override def getLockedRoot: Future[Option[Host.GUID]] = {
        doRequest(endpoint, TURN_GET_LOCKED_ROOT, withReceiver).map {
          case (_, root, _, _, _, _, _, _, success) =>
            if(success) Some(root) else None
        }(notWorthToMoveToTaskpool)
      }
      override def tryLock(): Future[TryLockResult] = {
        doRequest(endpoint, TURN_TRY_LOCK, withReceiver).map {
          case (_, newParent, _, _, _, _, _, _, success) =>
            TryLockResult(success, localLockInstance(newParent, endpoint))
        }(notWorthToMoveToTaskpool)
      }
      override def lock(): Future[SubsumableLock] = {
        doRequest(endpoint, TURN_LOCK, withReceiver).map {
          case (_, newParent, _, _, _, _, _, _, _) =>
            localLockInstance(newParent, endpoint)
        }(notWorthToMoveToTaskpool)
      }
      override def spinOnce(backoff: Host.GUID): Future[SubsumableLock] = {
        doRequest(endpoint, TURN_SPIN_ONCE, withReceiver.copy(_8 = backoff)).map {
          case (_, newParent, _, _, _, _, _, _, _) =>
            localLockInstance(newParent, endpoint)
        }(notWorthToMoveToTaskpool)
      }
      override def trySubsume(lockedNewParent: SubsumableLock): Future[Option[SubsumableLock]] = {
        assert(lockedNewParent.host == host.lockHost)
        doRequest(endpoint, TURN_TRY_SUBSUME, withReceiver.copy(_4 = lockedNewParent.guid)).map {
          case (_, newParent, _, _, _, _, _, _, success) =>
            if(success) None else Some(localLockInstance(newParent, endpoint))
        }(notWorthToMoveToTaskpool)
      }
    }

    class FullMVTurnReflectionProxyToEndpoint(val guid: Host.GUID, endpoint: EndPointWithInfrastructure[ParametersOrReturns]) extends FullMVTurnReflectionProxy {
      val withReceiver: ParametersOrReturns = allEmpty.copy(_2 = guid)
      override def newPredecessors(predecessors: Seq[GUID]): Future[Unit] = {
        doRequest(endpoint, NEW_PREDECESSORS, withReceiver.copy(_7 = predecessors)).map(_ => ())(notWorthToMoveToTaskpool)
      }

      override def newPhase(phase: TurnPhase.Type): Future[Unit] = {
        doRequest(endpoint, NEW_PHASE, withReceiver.copy(_5 = phase)).map(_ => ())(notWorthToMoveToTaskpool)
      }
    }

    class SubsumableLockMirrorProxyToEndpoint(val guid: Host.GUID, endpoint: EndPointWithInfrastructure[ParametersOrReturns]) extends SubsumableLockProxy {
      val withReceiver: ParametersOrReturns = allEmpty.copy(_2 = guid)
      override def subsume(lockedNewParent: SubsumableLock): Future[Unit] = {
        assert(lockedNewParent.host == host.lockHost)
        doRequest(endpoint, LOCK_SUBSUME, withReceiver.copy(_4 = lockedNewParent.guid)).map(_ => ())(notWorthToMoveToTaskpool)
      }
      override def unlock(): Future[SubsumableLock] = {
        doRequest(endpoint, LOCK_ASYNC_UNLOCK, withReceiver).map {
          case (_, newParent, _, _, _, _, _, _, _) =>
            localLockInstance(newParent, endpoint)
        }(notWorthToMoveToTaskpool)
      }
      override def getLockedRoot: Future[Option[Host.GUID]] = {
        doRequest(endpoint, LOCK_GET_LOCKED_ROOT, withReceiver).map {
          case (_, root, _, _, _, _, _, _, success) =>
            if(success) Some(root) else None
        }(notWorthToMoveToTaskpool)
      }
      override def tryLock(): Future[TryLockResult] = {
        doRequest(endpoint, LOCK_TRY_LOCK, withReceiver).map {
          case (_, newParent, _, _, _, _, _, _, success) =>
          TryLockResult(success, localLockInstance(newParent, endpoint))
        }(notWorthToMoveToTaskpool)
      }
      override def lock(): Future[SubsumableLock] = {
        doRequest(endpoint, LOCK_LOCK, withReceiver).map {
          case (_, newParent, _, _, _, _, _, _, _) =>
            localLockInstance(newParent, endpoint)
        }(notWorthToMoveToTaskpool)
      }
      override def spinOnce(backoff: Host.GUID): Future[SubsumableLock] = {
        doRequest(endpoint, LOCK_SPIN_ONCE, withReceiver.copy(_8 = backoff)).map {
          case (_, newParent, _, _, _, _, _, _, _) =>
            localLockInstance(newParent, endpoint)
        }(notWorthToMoveToTaskpool)
      }
      override def trySubsume(lockedNewParent: SubsumableLock): Future[Option[SubsumableLock]] = {
        assert(lockedNewParent.host == host.lockHost)
        doRequest(endpoint, LOCK_TRY_SUBSUME, withReceiver.copy(_4 = lockedNewParent.guid)).map {
          case (_, newParent, _, _, _, _, _, _, success) =>
            if(success) None else Some(localLockInstance(newParent, endpoint))
        }(notWorthToMoveToTaskpool)
      }
    }

    class ReactiveReflectionProxyToEndpoint(endpoint: EndPointWithInfrastructure[ParametersOrReturns]) extends ReactiveReflectionProxy[Pulse[P]] {
      override def asyncIncrementFrame(turn: FullMVTurn): Unit = {
        assert(turn.host == host)
        doAsync(endpoint, ASYNC_INCREMENT_FRAME, allEmpty.copy(_2 = turn.guid))
      }
      override def asyncIncrementSupersedeFrame(turn: FullMVTurn, supersede: FullMVTurn): Unit = {
        assert(turn.host == host)
        assert(supersede.host == host)
        doAsync(endpoint, ASYNC_INCREMENT_SUPERSEDE_FRAME, allEmpty.copy(_2 = turn.guid, _4 = supersede.guid))
      }
      override def asyncResolvedUnchanged(turn: FullMVTurn): Unit = {
        assert(turn.host == host)
        doAsync(endpoint, ASYNC_RESOLVE_UNCHANGED, allEmpty.copy(_2 = turn.guid))
      }
      override def asyncResolvedUnchangedFollowFrame(turn: FullMVTurn, followFrame: FullMVTurn): Unit = {
        assert(turn.host == host)
        assert(followFrame.host == host)
        doAsync(endpoint, ASYNC_RESOLVE_UNCHNAGED_FOLLOW_FRAME, allEmpty.copy(_2 = turn.guid, _4 = followFrame.guid))
      }
      override def asyncNewValue(turn: FullMVTurn, value: Pulse[P]): Unit = {
        assert(turn.host == host)
        doAsync(endpoint, ASYNC_NEW_VALUE, allEmpty.copy(_2 = turn.guid, _3 = Pluse.fromPulse(value)))
      }
      override def asyncNewValueFollowFrame(turn: FullMVTurn, value: Pulse[P], followFrame: FullMVTurn): Unit = {
        assert(turn.host == host)
        assert(followFrame.host == host)
        doAsync(endpoint, ASYNC_NEW_VALUE_FOLLOW_FRAME, allEmpty.copy(_2 = turn.guid, _3 = Pluse.fromPulse(value), _4 = followFrame.guid))
      }
    }
  }
}
