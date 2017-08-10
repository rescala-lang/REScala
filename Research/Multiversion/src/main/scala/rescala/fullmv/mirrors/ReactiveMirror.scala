package rescala.fullmv.mirrors

import java.util.concurrent.ConcurrentHashMap

import rescala.core.Node.InDep
import rescala.core._
import rescala.fullmv._
import rescala.reactives.{Event, Signal}

object ReactiveMirror {
  def createLocalPushClone[A](signal: Signal[A, FullMVStruct])(implicit ticket: CreationTicket[FullMVStruct]): Signal[A, FullMVStruct] = {
    val valuePersistency = ValuePersistency.DerivedSignal[A]
    ticket { creation =>
      creation.create(Set(), valuePersistency) { initialState =>
        val reflection = new LocalReactiveReflectionImpl[A](None, initialState, REName.fromString(s"SignalReflection(${ticket.rename.name})")) with Signal[A, FullMVStruct] {
          override def disconnect()(implicit engine: Engine[FullMVStruct]): Unit = ???
        }
        connectAndInitializeLocalPushClone(signal, creation /* fuckit */ .asInstanceOf[FullMVTurn], reflection, valuePersistency.isTransient, ticket.rename.name)
        reflection
      }
    }
  }

  def createLocalPushClone[P](event: Event[P, FullMVStruct])(implicit ticket: CreationTicket[FullMVStruct]): Event[P, FullMVStruct] = {
    val valuePersistency = ValuePersistency.Event[P]
    ticket { creation =>
      creation.create(Set(), valuePersistency) { initialState =>
        val turn = creation /* fuckit */ .asInstanceOf[FullMVTurn]
        val reflection = new LocalReactiveReflectionImpl[P](Some(turn), initialState, REName.fromString(s"EventReflection(${ticket.rename.name})")) with Event[P, FullMVStruct] {
          override def disconnect()(implicit engine: Engine[FullMVStruct]): Unit = ???
        }
        connectAndInitializeLocalPushClone(event, turn, reflection, valuePersistency.isTransient, ticket.rename.name)
        reflection
      }
    }
  }

  def connectAndInitializeLocalPushClone[A](reactive: ReadableReactive[Pulse[A], FullMVStruct], turn: FullMVTurn, reflection: LocalReactiveReflectionImpl[A], reflectionIsTransient: Boolean, name: String): Unit = {
    // simple remote interface for transfer in one direction
    val reflectionProxy: ReactiveReflectionProxy[Pulse[A]] = reflection
    // simple initialization data for transfer in the other direction
    val (initValues, maybeFirstFrame) = ReactiveMirror(reactive, turn, reflectionProxy, reflectionIsTransient, REName.fromString(s"Mirror($name)"))

    reflection.state.retrofitSinkFrames(initValues.map(_._1), maybeFirstFrame, +1)
    for((turn, value) <- initValues) reflection.buffer(turn, value)
  }

  def apply[A](reactive: ReadableReactive[A, FullMVStruct], turn: FullMVTurn, reflection: ReactiveReflectionProxy[A], reflectionIsTransient: Boolean, rename: REName): (Array[(FullMVTurn, A)], Option[FullMVTurn]) = {
    def getValue(turn: FullMVTurn): A = turn.staticAfter(reactive)
    val mirror = new ReactiveMirror(getValue, reflection.pushUpdate, rename)

    val (successorWrittenVersions, maybeFirstFrame) = reactive.state.discover(turn, mirror)
    val mustAddBaseValue = !reflectionIsTransient && (successorWrittenVersions.isEmpty || successorWrittenVersions.head != turn)
    var idx = if(mustAddBaseValue) 1 else 0
    val initValues = new Array[(FullMVTurn, A)](successorWrittenVersions.size + idx)
    if(mustAddBaseValue) initValues(0) = turn -> getValue(turn)
    for(succ <- successorWrittenVersions) {
      initValues(idx) = succ -> getValue(succ)
      idx += 1
    }
    (initValues, maybeFirstFrame)
  }
}

class ReactiveMirror[A](val getValue: FullMVTurn => A, val send: RemotePushMessage[A] => Unit, rename: REName) extends RENamed(rename) with Reactive[FullMVStruct] with FullMVState[Nothing, FullMVTurn, InDep[FullMVStruct], Reactive[FullMVStruct]] {
  override type Value = Nothing
  override protected[rescala] val state = this
  override def incrementSupersedeFrame(txn: FullMVTurn, supersede: FullMVTurn): FramingBranchResult[FullMVTurn, Reactive[FullMVStruct]] = {
    txn.activeBranchDifferential(TurnPhase.Framing, 1)
    send(RemotePushMessage.SupersedeFraming(txn, supersede))
    FramingBranchResult.FramingBranchEnd
  }
  override def incrementFrame(txn: FullMVTurn): FramingBranchResult[FullMVTurn, Reactive[FullMVStruct]] = {
    txn.activeBranchDifferential(TurnPhase.Framing, 1)
    send(RemotePushMessage.Framing(txn))
    FramingBranchResult.FramingBranchEnd
  }
  override def notify(txn: FullMVTurn, changed: Boolean): NotificationResultAction[FullMVTurn, Reactive[FullMVStruct]] = {
    txn.activeBranchDifferential(TurnPhase.Executing, 1)
    send(if(changed) RemotePushMessage.ChangeNotification(txn, getValue(txn)) else RemotePushMessage.UnchangedNotification(txn))
    NotificationResultAction.GlitchFreeReadyButQueued
  }
  override def notifyFollowFrame(txn: FullMVTurn, changed: Boolean, followFrame: FullMVTurn): NotificationResultAction[FullMVTurn, Reactive[FullMVStruct]] = {
    txn.activeBranchDifferential(TurnPhase.Executing, 1)
    send(if(changed) RemotePushMessage.ChangeNotificationWithFollowFrame(txn, getValue(txn), followFrame) else RemotePushMessage.UnchangedNotificationWithFollowFrame(txn, followFrame))
    NotificationResultAction.GlitchFreeReadyButQueued
  }
  override def latestValue: Value = ???
  override def reevIn(turn: FullMVTurn): Nothing = ???
  override def reevOut(turn: FullMVTurn, maybeValue: Option[Value]): NotificationResultAction.NotificationOutAndSuccessorOperation[FullMVTurn, Reactive[FullMVStruct]] = ???
  override def dynamicBefore(txn: FullMVTurn): Nothing = ???
  override def staticBefore(txn: FullMVTurn): Nothing = ???
  override def dynamicAfter(txn: FullMVTurn): Nothing = ???
  override def staticAfter(txn: FullMVTurn): Nothing = ???
  override def discover(txn: FullMVTurn, add: Reactive[FullMVStruct]): (Iterable[FullMVTurn], Option[FullMVTurn]) = ???
  override def drop(txn: FullMVTurn, remove: Reactive[FullMVStruct]): (Iterable[FullMVTurn], Option[FullMVTurn]) = ???
  override def retrofitSinkFrames(successorWrittenVersions: Iterable[FullMVTurn], maybeSuccessorFrame: Option[FullMVTurn], arity: Int): Unit = ???

  override protected[rescala] def reevaluate(turn: Turn[FullMVStruct], before: Value, indeps: Set[InDep[FullMVStruct]]): ReevaluationResult[FullMVStruct] = ???
}

trait ReactiveReflectionProxy[-P] {
  def pushUpdate(msg: RemotePushMessage[P]): Unit
}

trait LocalReactiveReflection[-P] extends WriteableReactive[P, FullMVStruct] with Reactive[FullMVStruct] with ReactiveReflectionProxy[P] {
  self: RENamed =>
  override def pushUpdate(msg: RemotePushMessage[P]): Unit = {
    FullMVEngine.threadPool.submit(msg.toTask(this))
  }
  def buffer(turn: FullMVTurn, value: P): Unit
}

class LocalReactiveReflectionImpl[P](var ignoreTurn: Option[FullMVTurn], initialState: FullMVState[Pulse[P], FullMVTurn, InDep[FullMVStruct], Reactive[FullMVStruct]], rename: REName) extends Base[P, FullMVStruct](initialState, rename) with LocalReactiveReflection[Pulse[P]] {
  val _buffer = new ConcurrentHashMap[FullMVTurn, Pulse[P]]()
  override def buffer(turn: FullMVTurn, value: Pulse[P]): Unit = _buffer.put(turn, value)

  override protected[rescala] def reevaluate(turn: Turn[FullMVStruct], before: Value, indeps: Set[InDep[FullMVStruct]]): ReevaluationResult[FullMVStruct] = {
    val value = _buffer.remove(turn)
    if(value == null) {
      if(ignoreTurn.contains(turn)){
        ignoreTurn = None
      } else {
        throw new AssertionError(s"$this was reevaluated for $turn but no value was buffered.")
      }
      ReevaluationResult.Static(turn, this, Pulse.NoChange: Pulse[P], indeps)
    } else {
      if(ignoreTurn.contains(turn)) ignoreTurn = None
      ReevaluationResult.Static(turn, this, value, indeps)
    }
  }
}
