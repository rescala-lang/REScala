package rescala.fullmv.transmitter

import java.util.concurrent.ConcurrentHashMap

import rescala.core.Node.InDep
import rescala.core._
import rescala.fullmv._
import rescala.reactives.Signal

import scala.collection.mutable.ArrayBuffer

object ReactiveMirror {
  def apply[A](signal: Signal[A, FullMVStruct])(implicit turn: FullMVTurn, rename: REName): Signal[A, FullMVStruct] = {
    var reflections = Set[Reflection[Pulse[A]]]()
    def send(msg: RemotePushMessage[Pulse[A]]): Int = {
      val r = reflections
      r.foreach(_.receivePushUpdate(msg))
      r.size
    }

    val mirror = new Reactive[FullMVStruct] with /* this is stupid, but necessary as long as ReevaluationResult has the commitTuple */ WriteableReactive[Nothing, FullMVStruct] {
      override protected[rescala] val state = new FullMVState[Value, FullMVTurn, InDep[FullMVStruct], Reactive[FullMVStruct]] {
        override def incrementSupersedeFrame(txn: FullMVTurn, supersede: FullMVTurn): FramingBranchResult[FullMVTurn, Reactive[FullMVStruct]] = {
          txn.activeBranchDifferential(TurnPhase.Framing, send(SupersedeFraming(txn, supersede)))
          FramingBranchResult.FramingBranchEnd
        }
        override def incrementFrame(txn: FullMVTurn): FramingBranchResult[FullMVTurn, Reactive[FullMVStruct]] = {
          txn.activeBranchDifferential(TurnPhase.Framing, send(Framing(txn)))
          FramingBranchResult.FramingBranchEnd
        }
        override def notify(txn: FullMVTurn, changed: Boolean): NotificationResultAction[FullMVTurn, Reactive[FullMVStruct]] = {
          txn.activeBranchDifferential(TurnPhase.Executing, send(if(changed) ChangeNotification(txn, txn.staticAfter(signal)) else UnchangedNotification(txn)))
          NotificationResultAction.GlitchFreeReadyButQueued
        }
        override def notifyFollowFrame(txn: FullMVTurn, changed: Boolean, followFrame: FullMVTurn): NotificationResultAction[FullMVTurn, Reactive[FullMVStruct]] = {
          txn.activeBranchDifferential(TurnPhase.Executing, send(if(changed) ChangeNotificationWithFollowFrame(txn, txn.staticAfter(signal), followFrame) else UnchangedNotificationWithFollowFrame(txn, followFrame)))
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

    val (successorWrittenVersions, maybeFirstFrame) = signal.state.discover(turn, mirror)
    val initVersions: ArrayBuffer[(FullMVTurn, Pulse.Change[A])] = new ArrayBuffer(successorWrittenVersions.size + 1)
    if(successorWrittenVersions.head != turn) {
      initVersions += turn -> turn.staticAfter(signal).asInstanceOf[Pulse.Change[A]]
    }
    for(succTurn <- successorWrittenVersions) {
      initVersions += succTurn -> succTurn.staticAfter(signal).asInstanceOf[Pulse.Change[A]]
    }

    val reflection = new ReflectionImpl[A](initVersions, maybeFirstFrame, ValuePersistency.DerivedSignal[A], rename) with Signal[A, FullMVStruct] {
      override def disconnect()(implicit engine: Engine[FullMVStruct]): Unit = ???
    }
    reflections += reflection

    reflection
  }
}

trait Reflection[-P] extends WriteableReactive[P, FullMVStruct] with Reactive[FullMVStruct] {
  def receivePushUpdate(msg: RemotePushMessage[P]): Unit = {
    FullMVEngine.threadPool.submit(msg.toTask(this))
  }
  def buffer(turn: FullMVTurn, value: P): Unit
}

class ReflectionImpl[P](initVersions: Iterable[(FullMVTurn, Pulse[P])], maybeFirstFrame: Option[FullMVTurn], valuePersistency: ValuePersistency[Pulse[P]], rename: REName) extends Base[P, FullMVStruct](new NodeVersionHistory(initVersions.head._1, valuePersistency), rename) with Reflection[Pulse[P]] {
  val _buffer = new ConcurrentHashMap[FullMVTurn, Pulse[P]]()
  override def buffer(turn: FullMVTurn, value: Pulse[P]): Unit = _buffer.put(turn, value)

  // TODO correct initialization
  state.retrofitSinkFrames(initVersions.tail.map(_._1), maybeFirstFrame, +1)

  override protected[rescala] def reevaluate(turn: Turn[FullMVStruct], before: Value, indeps: Set[InDep[FullMVStruct]]): ReevaluationResult[FullMVStruct] = {
    ReevaluationResult.Static(turn, this, _buffer.remove(turn), indeps)
  }
}
