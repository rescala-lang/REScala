package rescala.fullmv.mirrors

import rescala.core._
import rescala.fullmv.{FullMVStruct, FullMVTurn}
import rescala.reactives.{Event, Signal}

object ReactiveLocalClone {
  def apply[A](signal: Signal[A, FullMVStruct])(implicit ticket: CreationTicket[FullMVStruct]): Signal[A, FullMVStruct] = {
    val valuePersistency = ValuePersistency.DerivedSignal[A]
    ticket { creation =>
      creation.create(Set(), valuePersistency) { initialState =>
        val reflection = new ReactiveReflectionImpl[A](None, initialState, ticket.rename.derive("SignalReflection")) with Signal[A, FullMVStruct] {
          override def disconnect()(implicit engine: Engine[FullMVStruct]): Unit = ???
        }
        connectAndInitializeLocalPushClone(signal, creation /* fuckit */ .asInstanceOf[FullMVTurn], reflection, valuePersistency.isTransient, ticket.rename.name)
        reflection
      }
    }
  }

  def apply[P](event: Event[P, FullMVStruct])(implicit ticket: CreationTicket[FullMVStruct]): Event[P, FullMVStruct] = {
    val valuePersistency = ValuePersistency.Event[P]
    ticket { creation =>
      creation.create(Set(), valuePersistency) { initialState =>
        val turn = creation /* fuckit */ .asInstanceOf[FullMVTurn]
        val reflection = new ReactiveReflectionImpl[P](Some(turn), initialState, ticket.rename.derive("EventReflection")) with Event[P, FullMVStruct] {
          override def disconnect()(implicit engine: Engine[FullMVStruct]): Unit = ???
        }
        connectAndInitializeLocalPushClone(event, turn, reflection, valuePersistency.isTransient, ticket.rename.name)
        reflection
      }
    }
  }

  def connectAndInitializeLocalPushClone[A](reactive: ReadableReactive[Pulse[A], FullMVStruct], turn: FullMVTurn, reflection: ReactiveReflectionImpl[A], reflectionIsTransient: Boolean, rename: REName): Unit = {
    // simple remote interface for transfer in one direction
    val reflectionProxy: ReactiveReflectionProxy[Pulse[A]] = new ReactiveReflectionProxy[Pulse[A]] {
      override def asyncIncrementFrame(turn: FullMVTurn): Unit = reflection.asyncIncrementFrame(FullMVTurnLocalClone.apply(turn))
      override def asyncIncrementSupersedeFrame(turn: FullMVTurn, supersede: FullMVTurn): Unit = reflection.asyncIncrementSupersedeFrame(FullMVTurnLocalClone.apply(turn), FullMVTurnLocalClone.apply(supersede))
      override def asyncNewValue(turn: FullMVTurn, value: Pulse[A]): Unit = reflection.asyncNewValue(FullMVTurnLocalClone.apply(turn), value)
      override def asyncResolvedUnchanged(turn: FullMVTurn): Unit = reflection.asyncResolvedUnchanged(FullMVTurnLocalClone.apply(turn))
      override def asyncResolvedUnchangedFollowFrame(turn: FullMVTurn, followFrame: FullMVTurn): Unit = reflection.asyncResolvedUnchangedFollowFrame(FullMVTurnLocalClone.apply(turn), FullMVTurnLocalClone.apply(followFrame))
      override def asyncNewValueFollowFrame(turn: FullMVTurn, value: Pulse[A], followFrame: FullMVTurn): Unit = reflection.asyncNewValueFollowFrame(FullMVTurnLocalClone.apply(turn), value, FullMVTurnLocalClone.apply(followFrame))
    }
    // simple initialization data for transfer in the other direction
    val (initValues, maybeFirstFrame) = ReactiveMirror(reactive, turn, reflectionProxy, reflectionIsTransient, rename.derive("Mirror"))

    reflection.state.retrofitSinkFrames(initValues.map(_._1), maybeFirstFrame, +1)
    for((turn, value) <- initValues) reflection.buffer(turn, value)
  }
}
