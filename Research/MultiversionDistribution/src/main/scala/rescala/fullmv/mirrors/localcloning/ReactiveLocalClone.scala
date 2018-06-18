package rescala.fullmv.mirrors.localcloning

import rescala.core._
import rescala.fullmv.mirrors.{ReactiveMirror, ReactiveReflectionImpl, ReactiveReflectionProxy}
import rescala.fullmv.{FullMVEngine, FullMVStruct, FullMVTurn}
import rescala.reactives.{Event, Signal}

import scala.concurrent.duration.Duration

object ReactiveLocalClone {
  def apply[A](signal: Signal[A, FullMVStruct], host: FullMVEngine)(implicit name: REName): Signal[A, FullMVStruct] = apply(signal, host, Duration.Zero)(name)
  def apply[A](signal: Signal[A, FullMVStruct], host: FullMVEngine, fakeDelay: Duration)(implicit name: REName): Signal[A, FullMVStruct] = apply(signal, fakeDelay)(CreationTicket.fromEngine(host)(name))
  def apply[A](signal: Signal[A, FullMVStruct])(implicit ticket: CreationTicket[FullMVStruct]): Signal[A, FullMVStruct] = apply(signal, Duration.Zero)(ticket)
  def apply[A](signal: Signal[A, FullMVStruct], fakeDelay: Duration)(implicit ticket: CreationTicket[FullMVStruct]): Signal[A, FullMVStruct] = {
    ticket { creation =>
      val turn = creation /* fuckit */ .asInstanceOf[FullMVTurn]
      creation.create(Set(), Initializer.DerivedSignal[A], inite = true) { initialState =>
        val reflection = new ReactiveReflectionImpl[Pulse[A]](turn.host, None, initialState, ticket.rename.derive("SignalReflection")) with Signal[A, FullMVStruct] {
          override def disconnect()(implicit engine: Scheduler[FullMVStruct]): Unit = ???
        }
        connectAndInitializeLocalPushClone(fakeDelay, signal, turn, reflectionIsTransient = false, ticket.rename.name)(identity, reflection)
        reflection
      }
    }
  }

  def apply[P](event: Event[P, FullMVStruct], host: FullMVEngine)(implicit name: REName): Event[P, FullMVStruct] = apply(event, host, Duration.Zero)(name)
  def apply[P](event: Event[P, FullMVStruct], host: FullMVEngine, fakeDelay: Duration)(implicit name: REName): Event[P, FullMVStruct] = apply(event, fakeDelay)(CreationTicket.fromEngine(host)(name))
  def apply[P](event: Event[P, FullMVStruct])(implicit ticket: CreationTicket[FullMVStruct]): Event[P, FullMVStruct] = apply(event, Duration.Zero)(ticket)
  def apply[P](event: Event[P, FullMVStruct], fakeDelay: Duration)(implicit ticket: CreationTicket[FullMVStruct]): Event[P, FullMVStruct] = {
    ticket { creation =>
      creation.create(Set(), Initializer.Event[P], inite = false) { initialState =>
        val turn = creation /* fuckit */ .asInstanceOf[FullMVTurn]
        val reflection = new ReactiveReflectionImpl[Pulse[P]](turn.host, Some(turn), initialState, ticket.rename.derive("EventReflection")) with Event[P, FullMVStruct] {
          override def internalAccess(v: Pulse[P]): Pulse[P] = v
          override def disconnect()(implicit engine: Scheduler[FullMVStruct]): Unit = ???
        }
        connectAndInitializeLocalPushClone(fakeDelay, event, turn, reflectionIsTransient = true, ticket.rename.name)(event.internalAccess, reflection)
        reflection
      }
    }
  }

  def connectAndInitializeLocalPushClone[A](fakeDelay: Duration, reactive: ReSource[FullMVStruct], connectTurn: FullMVTurn, reflectionIsTransient: Boolean, rename: REName)(toPulse: reactive.Value => A, reflection: ReactiveReflectionImpl[A]): Unit = {
    if (FullMVEngine.DEBUG) println(s"[${Thread.currentThread().getName}] $connectTurn creating clone of $reactive")
    val reflectionHost = connectTurn.host
    // simple remote interface for transfer in one direction
    val reflectionProxy: ReactiveReflectionProxy[A] = new ReactiveReflectionProxy[A] {
      override def asyncIncrementFrame(turn: FullMVTurn): Unit = FakeDelayer.async(fakeDelay, reflection.asyncIncrementFrame(FullMVTurnLocalClone(turn, reflectionHost, fakeDelay)))
      override def asyncIncrementSupersedeFrame(turn: FullMVTurn, supersede: FullMVTurn): Unit = FakeDelayer.async(fakeDelay, reflection.asyncIncrementSupersedeFrame(FullMVTurnLocalClone(turn, reflectionHost, fakeDelay), FullMVTurnLocalClone(supersede, reflectionHost, fakeDelay)))
      override def asyncNewValue(turn: FullMVTurn, value: A): Unit = FakeDelayer.async(fakeDelay, reflection.asyncNewValue(FullMVTurnLocalClone(turn, reflectionHost, fakeDelay), value))
      override def asyncResolvedUnchanged(turn: FullMVTurn): Unit = FakeDelayer.async(fakeDelay, reflection.asyncResolvedUnchanged(FullMVTurnLocalClone(turn, reflectionHost, fakeDelay)))
      override def asyncResolvedUnchangedFollowFrame(turn: FullMVTurn, followFrame: FullMVTurn): Unit = FakeDelayer.async(fakeDelay, reflection.asyncResolvedUnchangedFollowFrame(FullMVTurnLocalClone(turn, reflectionHost, fakeDelay), FullMVTurnLocalClone(followFrame, reflectionHost, fakeDelay)))
      override def asyncNewValueFollowFrame(turn: FullMVTurn, value: A, followFrame: FullMVTurn): Unit = FakeDelayer.async(fakeDelay, reflection.asyncNewValueFollowFrame(FullMVTurnLocalClone(turn, reflectionHost, fakeDelay), value, FullMVTurnLocalClone(followFrame, reflectionHost, fakeDelay)))
    }
    // simple initialization data for transfer in the other direction
    val (mirrorInitValues, mirrorMaybeFirstFrame) = ReactiveMirror(reactive, FullMVTurnLocalClone(connectTurn, reactive.state.host, fakeDelay), reflectionIsTransient, rename.derive("Mirror"))(toPulse, reflectionProxy)

    if(fakeDelay != Duration.Zero) {
      // Connect message transfer
      val firstWait = System.currentTimeMillis() + fakeDelay.toMillis
      while (firstWait < System.currentTimeMillis()) Thread.sleep(1)
    }

    val reflectionInitValues = mirrorInitValues.map{ case (mirrorTurn, value) => FullMVTurnLocalClone(mirrorTurn, reflectionHost, fakeDelay) -> value }
    val reflectionMaybeFirstFrame = mirrorMaybeFirstFrame.map((turn: FullMVTurn) => FullMVTurnLocalClone(turn, reflectionHost, fakeDelay))

    if(fakeDelay != Duration.Zero) {
      // Initialize message transfer
      val secondWait = System.currentTimeMillis() + fakeDelay.toMillis
      while (secondWait < System.currentTimeMillis()) Thread.sleep(1)
    }

    reflection.state.retrofitSinkFrames(reflectionInitValues.map(_._1), reflectionMaybeFirstFrame, +1)
    for((reflectionTurn, value) <- reflectionInitValues) reflection.buffer(reflectionTurn, value)
  }
}
