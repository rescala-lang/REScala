package rescala.fullmv.mirrors.localcloning

import rescala.core._
import rescala.fullmv.mirrors.{ReactiveMirror, ReactiveReflectionImpl, ReactiveReflectionProxy}
import rescala.fullmv.{FullMVEngine, FullMVStruct, FullMVTurn}
import rescala.reactives.{Event, Signal}

object ReactiveLocalClone {
  def apply[A](signal: Signal[A, FullMVStruct], host: FullMVEngine)(implicit name: REName): Signal[A, FullMVStruct] = apply(signal)(CreationTicket.fromEngine(host)(name))
  def apply[A](signal: Signal[A, FullMVStruct])(implicit ticket: CreationTicket[FullMVStruct]): Signal[A, FullMVStruct] = {
    ticket { creation =>
      val turn = creation /* fuckit */ .asInstanceOf[FullMVTurn]
      creation.create(Set(), Initializer.DerivedSignal[A], inite = true) { initialState =>
        val reflection = new ReactiveReflectionImpl[Pulse[A]](turn.host, None, initialState, ticket.rename.derive("SignalReflection")) with Signal[A, FullMVStruct] {
          override def disconnect()(implicit engine: Scheduler[FullMVStruct]): Unit = ???
        }
        connectAndInitializeLocalPushClone(signal, turn, reflectionIsTransient = false, ticket.rename.name)(identity, reflection)
        reflection
      }
    }
  }

  def apply[P](event: Event[P, FullMVStruct], host: FullMVEngine)(implicit name: REName): Event[P, FullMVStruct] = apply(event)(CreationTicket.fromEngine(host)(name))
  def apply[P](event: Event[P, FullMVStruct])(implicit ticket: CreationTicket[FullMVStruct]): Event[P, FullMVStruct] = {
    ticket { creation =>
      creation.create(Set(), Initializer.Event[P], inite = false) { initialState =>
        val turn = creation /* fuckit */ .asInstanceOf[FullMVTurn]
        val reflection = new ReactiveReflectionImpl[Pulse[P]](turn.host, Some(turn), initialState, ticket.rename.derive("EventReflection")) with Event[P, FullMVStruct] {
          override def internalAccess(v: Pulse[P]): Pulse[P] = v
          override def disconnect()(implicit engine: Scheduler[FullMVStruct]): Unit = ???
        }
        connectAndInitializeLocalPushClone(event, turn, reflectionIsTransient = true, ticket.rename.name)(event.internalAccess, reflection)
        reflection
      }
    }
  }

  def connectAndInitializeLocalPushClone[A](reactive: ReSource[FullMVStruct], connectTurn: FullMVTurn, reflectionIsTransient: Boolean, rename: REName)(toPulse: reactive.Value => A, reflection: ReactiveReflectionImpl[A]): Unit = {
    if (FullMVEngine.DEBUG) println(s"[${Thread.currentThread().getName}] $connectTurn creating clone of $reactive")
    val reflectionHost = connectTurn.host
    // simple remote interface for transfer in one direction
    val reflectionProxy: ReactiveReflectionProxy[A] = new ReactiveReflectionProxy[A] {
      override def asyncIncrementFrame(turn: FullMVTurn): Unit = reflection.asyncIncrementFrame(FullMVTurnLocalClone(turn, reflectionHost))
      override def asyncDecrementFrame(turn: FullMVTurn): Unit = reflection.asyncDecrementFrame(FullMVTurnLocalClone(turn, reflectionHost))
      override def asyncIncrementSupersedeFrame(turn: FullMVTurn, supersede: FullMVTurn): Unit = reflection.asyncIncrementSupersedeFrame(FullMVTurnLocalClone(turn, reflectionHost), FullMVTurnLocalClone(supersede, reflectionHost))
      override def asyncDeframeReframe(turn: FullMVTurn, reframe: FullMVTurn): Unit = reflection.asyncDeframeReframe(FullMVTurnLocalClone(turn, reflectionHost), FullMVTurnLocalClone(reframe, reflectionHost))
      override def asyncNewValue(turn: FullMVTurn, value: A): Unit = reflection.asyncNewValue(FullMVTurnLocalClone(turn, reflectionHost), value)
      override def asyncResolvedUnchanged(turn: FullMVTurn): Unit = reflection.asyncResolvedUnchanged(FullMVTurnLocalClone(turn, reflectionHost))
      override def asyncResolvedUnchangedFollowFrame(turn: FullMVTurn, followFrame: FullMVTurn): Unit = reflection.asyncResolvedUnchangedFollowFrame(FullMVTurnLocalClone(turn, reflectionHost), FullMVTurnLocalClone(followFrame, reflectionHost))
      override def asyncNewValueFollowFrame(turn: FullMVTurn, value: A, followFrame: FullMVTurn): Unit = reflection.asyncNewValueFollowFrame(FullMVTurnLocalClone(turn, reflectionHost), value, FullMVTurnLocalClone(followFrame, reflectionHost))
    }
    // simple initialization data for transfer in the other direction
    val (mirrorInitValues, mirrorMaybeFirstFrame) = ReactiveMirror(reactive, FullMVTurnLocalClone(connectTurn, reactive.state.host), reflectionIsTransient, rename.derive("Mirror"))(toPulse, reflectionProxy)

    val reflectionInitValues = mirrorInitValues.map{ case (mirrorTurn, value) => FullMVTurnLocalClone(mirrorTurn, reflectionHost) -> value }
    val reflectionMaybeFirstFrame = mirrorMaybeFirstFrame.map(FullMVTurnLocalClone(_, reflectionHost))

    reflection.state.retrofitSinkFrames(reflectionInitValues.map(_._1), reflectionMaybeFirstFrame, +1)
    for((reflectionTurn, value) <- reflectionInitValues) reflection.buffer(reflectionTurn, value)
  }
}
