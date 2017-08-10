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
    val reflectionProxy: ReactiveReflectionProxy[Pulse[A]] = reflection
    // simple initialization data for transfer in the other direction
    val (initValues, maybeFirstFrame) = ReactiveMirror(reactive, turn, reflectionProxy, reflectionIsTransient, rename.derive("Mirror"))

    reflection.state.retrofitSinkFrames(initValues.map(_._1), maybeFirstFrame, +1)
    for((turn, value) <- initValues) reflection.buffer(turn, value)
  }
}
