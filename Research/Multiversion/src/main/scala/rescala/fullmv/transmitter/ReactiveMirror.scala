package rescala.fullmv.transmitter

import java.util.concurrent.ConcurrentHashMap

import rescala.core.Node.InDep
import rescala.core._
import rescala.fullmv._
import rescala.reactives.{Event, Signal}

import scala.collection.mutable.ArrayBuffer

object ReactiveMirror {
  def apply[A](signal: Signal[A, FullMVStruct])(implicit ticket: CreationTicket[FullMVStruct]): Signal[A, FullMVStruct] = {
    val valuePersistency = ValuePersistency.DerivedSignal[A]
    ticket { creation =>
      creation.create(Set(), valuePersistency) { initialState =>
        val reflection = new LocalReflectionImpl[A](initialState, REName.fromString(s"SignalReflection(${ticket.rename.name})")) with Signal[A, FullMVStruct] {
          override def disconnect()(implicit engine: Engine[FullMVStruct]): Unit = ???
        }
        establishConnection(signal, creation /* fuckit */ .asInstanceOf[FullMVTurn], reflection, valuePersistency.isTransient, ticket.rename.name)
        reflection
      }
    }
  }

  def apply[P](event: Event[P, FullMVStruct])(implicit ticket: CreationTicket[FullMVStruct]): Event[P, FullMVStruct] = {
    val valuePersistency = ValuePersistency.Event[P]
    ticket { creation =>
      creation.create(Set(), valuePersistency) { initialState =>
        val reflection = new LocalReflectionImpl[P](initialState, REName.fromString(s"EventReflection(${ticket.rename.name})")) with Event[P, FullMVStruct] {
          override def disconnect()(implicit engine: Engine[FullMVStruct]): Unit = ???
        }
        establishConnection(event, creation /* fuckit */ .asInstanceOf[FullMVTurn], reflection, valuePersistency.isTransient, ticket.rename.name)
        reflection
      }
    }
  }

  def establishConnection[A](signal: ReadableReactive[Pulse[A], FullMVStruct], turn: FullMVTurn, reflection: LocalReflectionImpl[A], reflectionIsTransient: Boolean, name: String) = {
    val reflectionProxy: ReflectionProxy[Pulse[A]] = reflection

    val (initVersions, maybeFirstFrame: Option[FullMVTurn]) = attachMirror(signal, turn, reflectionProxy, reflectionIsTransient, REName.fromString(s"Mirror($name)"))

    for ((turn, value) <- initVersions) {
      reflection.state.incrementFrame(turn)
      reflection.buffer(turn, value)
      reflection.state.notify(turn, changed = true)
    }
    if (maybeFirstFrame.isDefined) {
      reflection.state.incrementFrame(maybeFirstFrame.get)
    }
  }

  def attachMirror[A](reactive: ReadableReactive[A, FullMVStruct], turn: FullMVTurn, reflection: ReflectionProxy[A], reflectionIsTransient: Boolean, rename: REName): (Iterable[(FullMVTurn, A)], Option[FullMVTurn]) = {
    def send(msg: RemotePushMessage[A]): Unit = {
      reflection.pushUpdate(msg)
    }

    val mirror = new Reactive[FullMVStruct] {
      override type Value = Nothing
      override protected[rescala] val state = new FullMVState[Value, FullMVTurn, InDep[FullMVStruct], Reactive[FullMVStruct]] {
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
          send(if(changed) RemotePushMessage.ChangeNotification(txn, txn.staticAfter(reactive)) else RemotePushMessage.UnchangedNotification(txn))
          NotificationResultAction.GlitchFreeReadyButQueued
        }
        override def notifyFollowFrame(txn: FullMVTurn, changed: Boolean, followFrame: FullMVTurn): NotificationResultAction[FullMVTurn, Reactive[FullMVStruct]] = {
          txn.activeBranchDifferential(TurnPhase.Executing, 1)
          send(if(changed) RemotePushMessage.ChangeNotificationWithFollowFrame(txn, txn.staticAfter(reactive), followFrame) else RemotePushMessage.UnchangedNotificationWithFollowFrame(txn, followFrame))
          NotificationResultAction.GlitchFreeReadyButQueued
        }
        override def latestValue: Value = ???
        override def reevIn(turn: FullMVTurn): Nothing = ???
        override def reevOut(turn: FullMVTurn, maybeValue: Option[Value]): NotificationResultAction.NotificationOutAndSuccessorOperation[FullMVTurn, Reactive[FullMVStruct]] = ???
        override def dynamicBefore(txn: FullMVTurn): Nothing = ???
        override def staticBefore(txn: FullMVTurn): Nothing = ???
        override def dynamicAfter(txn: FullMVTurn): Nothing = ???
        override def staticAfter(txn: FullMVTurn): Nothing = ???
        override def discover(txn: FullMVTurn, add: Reactive[FullMVStruct]): (ArrayBuffer[FullMVTurn], Option[FullMVTurn]) = ???
        override def drop(txn: FullMVTurn, remove: Reactive[FullMVStruct]): (ArrayBuffer[FullMVTurn], Option[FullMVTurn]) = ???
        override def retrofitSinkFrames(successorWrittenVersions: Iterable[FullMVTurn], maybeSuccessorFrame: Option[FullMVTurn], arity: Int): Unit = ???
      }
      override protected[rescala] def reevaluate(turn: Turn[FullMVStruct], before: Value, indeps: Set[InDep[FullMVStruct]]): ReevaluationResult[FullMVStruct] = ???
    }

    val (successorWrittenVersions, maybeFirstFrame) = reactive.state.discover(turn, mirror)
    val initVersions: ArrayBuffer[(FullMVTurn, A)] = new ArrayBuffer(successorWrittenVersions.size + 1)
    if(successorWrittenVersions.isEmpty || successorWrittenVersions.head != turn) {
      initVersions += turn -> turn.staticAfter(reactive)
    }
    for(succTurn <- successorWrittenVersions) {
      initVersions += succTurn -> succTurn.staticAfter(reactive)
    }
    (initVersions, maybeFirstFrame)
  }
}

trait ReflectionProxy[-P] {
  def pushUpdate(msg: RemotePushMessage[P]): Unit
}

trait LocalReflection[-P] extends WriteableReactive[P, FullMVStruct] with Reactive[FullMVStruct] with ReflectionProxy[P] {
  override def pushUpdate(msg: RemotePushMessage[P]): Unit = {
    FullMVEngine.threadPool.submit(msg.toTask(this))
  }
  def buffer(turn: FullMVTurn, value: P): Unit
}

class LocalReflectionImpl[P](initialState: FullMVState[Pulse[P], FullMVTurn, InDep[FullMVStruct], Reactive[FullMVStruct]], rename: REName) extends Base[P, FullMVStruct](initialState, rename) with LocalReflection[Pulse[P]] {
  val _buffer = new ConcurrentHashMap[FullMVTurn, Pulse[P]]()
  override def buffer(turn: FullMVTurn, value: Pulse[P]): Unit = _buffer.put(turn, value)

  override protected[rescala] def reevaluate(turn: Turn[FullMVStruct], before: Value, indeps: Set[InDep[FullMVStruct]]): ReevaluationResult[FullMVStruct] = {
    ReevaluationResult.Static(turn, this, _buffer.remove(turn), indeps)
  }
}
