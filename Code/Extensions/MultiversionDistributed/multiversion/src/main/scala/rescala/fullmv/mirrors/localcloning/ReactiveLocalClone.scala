package rescala.fullmv.mirrors.localcloning

import rescala.compat.SignalCompatBundle
import rescala.core._
import rescala.fullmv._
import rescala.fullmv.mirrors.{Mirror, ReactiveMirrorBundle, ReactiveReflectionBundle}
import rescala.fullmv.sgt.synchronization.SubsumableLockBundle
import rescala.fullmv.tasks.TaskBundle
import rescala.interface.RescalaInterface
import rescala.operator.{EventBundle, ObserveBundle, Pulse, SignalBundle}

import scala.concurrent.duration.Duration

trait ReactiveLocalCloneBundle extends FullMVBundle with SignalBundle {
  selfType: Mirror with TurnImplBundle with TaskBundle with FullMvStateBundle with SubsumableLockBundle with EventBundle
    with ReactiveReflectionBundle with FullMVTurnLocalCloneBundle with ReactiveMirrorBundle with RescalaInterface
    with SignalCompatBundle with EventBundle with SignalBundle with ObserveBundle =>

  object ReactiveLocalClone {
    def apply[A](signal: Signal[A], host: FullMVEngine)(implicit name: ReInfo): Signal[A] =
      apply(signal, host, Duration.Zero)(name)
    def apply[A](signal: Signal[A], host: FullMVEngine, fakeDelay: Duration)(implicit
        name: ReInfo
    ): Signal[A] = apply(signal, fakeDelay)(CreationTicket.fromExplicitDynamicScope(host)(name))
    def apply[A](signal: Signal[A])(implicit
        ticket: CreationTicket
    ): Signal[A] = apply(signal, Duration.Zero)(ticket)
    def apply[A](signal: Signal[A], fakeDelay: Duration)(implicit
        ticket: CreationTicket
    ): Signal[A] = {
      ticket.create(Set(), Pulse.empty: Pulse[A], needsReevaluation = true) { initialState =>
        val turn = initialState.asInstanceOf[
          NonblockingSkipListVersionHistory[_, FullMVTurn]
        ].laggingLatestStable.get().get().txn
        val reflection =
          new ReactiveReflectionImpl[Pulse[A]](turn.host, None, initialState, ticket.rename.derive("SignalReflection"))
            with Signal[A] {
            override def disconnect(): Unit = ???
          }
        connectAndInitializeLocalPushClone(
          fakeDelay,
          signal,
          turn,
          reflectionIsTransient = false,
          ReInfo.create.derive(ticket.rename.description)
        )(
          identity,
          reflection
        )
        reflection
      }
    }

    def apply[P](event: Event[P], host: FullMVEngine)(implicit name: ReInfo): Event[P] =
      apply(event, host, Duration.Zero)(name)
    def apply[P](event: Event[P], host: FullMVEngine, fakeDelay: Duration)(implicit
        name: ReInfo
    ): Event[P] = apply(event, fakeDelay)(CreationTicket.fromExplicitDynamicScope(host)(name))
    def apply[P](event: Event[P])(implicit ticket: CreationTicket): Event[P] =
      apply(event, Duration.Zero)(ticket)
    def apply[P](event: Event[P], fakeDelay: Duration)(implicit
        ticket: CreationTicket
    ): Event[P] = {
      ticket.create(Set(), Pulse.NoChange: Pulse[P], needsReevaluation = false) { initialState =>
        val turn = initialState.asInstanceOf[
          NonblockingSkipListVersionHistory[_, FullMVTurn]
        ].laggingLatestStable.get().get().txn
        val reflection = new ReactiveReflectionImpl[Pulse[P]](
          turn.host,
          Some(turn),
          initialState,
          ticket.rename.derive("EventReflection")
        ) with Event[P] {
          override def internalAccess(v: Pulse[P]): Pulse[P] = v
          override def disconnect(): Unit                    = ???
        }
        connectAndInitializeLocalPushClone(
          fakeDelay,
          event,
          turn,
          reflectionIsTransient = true,
          ReInfo.create.derive(ticket.rename.description)
        )(
          event.internalAccess,
          reflection
        )
        reflection
      }
    }

    def connectAndInitializeLocalPushClone[A](
        fakeDelay: Duration,
        reactive: ReSource,
        connectTurn: FullMVTurn,
        reflectionIsTransient: Boolean,
        rename: ReInfo
    )(toPulse: reactive.Value => A, reflection: ReactiveReflectionImpl[A]): Unit = {
      if (FullMVUtil.DEBUG) println(s"[${Thread.currentThread().getName}] $connectTurn creating clone of $reactive")
      val mirrorHost     = reactive.state.host
      val reflectionHost = connectTurn.host
      // simple remote interface for transfer in one direction
      val reflectionProxy: ReactiveReflectionProxy[A] = new ReactiveReflectionProxy[A] {
        override def asyncIncrementFrame(turn: FullMVTurn): Unit =
          FakeDelayer.async(
            mirrorHost,
            reflectionHost,
            fakeDelay,
            reflection.asyncIncrementFrame(FullMVTurnLocalClone.withPredecessorReplication(
              turn,
              reflectionHost,
              fakeDelay
            ))
          )
        override def asyncIncrementSupersedeFrame(turn: FullMVTurn, supersede: FullMVTurn): Unit =
          FakeDelayer.async(
            mirrorHost,
            reflectionHost,
            fakeDelay,
            reflection.asyncIncrementSupersedeFrame(
              FullMVTurnLocalClone.withPredecessorReplication(turn, reflectionHost, fakeDelay),
              FullMVTurnLocalClone.withPredecessorReplication(supersede, reflectionHost, fakeDelay)
            )
          )
        override def asyncNewValue(turn: FullMVTurn, value: A): Unit =
          FakeDelayer.async(
            mirrorHost,
            reflectionHost,
            fakeDelay,
            reflection.asyncNewValue(
              FullMVTurnLocalClone.withPredecessorReplication(turn, reflectionHost, fakeDelay),
              value
            )
          )
        override def asyncResolvedUnchanged(turn: FullMVTurn): Unit =
          FakeDelayer.async(
            mirrorHost,
            reflectionHost,
            fakeDelay,
            reflection.asyncResolvedUnchanged(FullMVTurnLocalClone.withPredecessorReplication(
              turn,
              reflectionHost,
              fakeDelay
            ))
          )
        override def asyncResolvedUnchangedFollowFrame(turn: FullMVTurn, followFrame: FullMVTurn): Unit =
          FakeDelayer.async(
            mirrorHost,
            reflectionHost,
            fakeDelay,
            reflection.asyncResolvedUnchangedFollowFrame(
              FullMVTurnLocalClone.withPredecessorReplication(turn, reflectionHost, fakeDelay),
              FullMVTurnLocalClone.withPredecessorReplication(followFrame, reflectionHost, fakeDelay)
            )
          )
        override def asyncNewValueFollowFrame(turn: FullMVTurn, value: A, followFrame: FullMVTurn): Unit =
          FakeDelayer.async(
            mirrorHost,
            reflectionHost,
            fakeDelay,
            reflection.asyncNewValueFollowFrame(
              FullMVTurnLocalClone.withPredecessorReplication(turn, reflectionHost, fakeDelay),
              value,
              FullMVTurnLocalClone.withPredecessorReplication(followFrame, reflectionHost, fakeDelay)
            )
          )
      }

      if (fakeDelay != Duration.Zero) {
        if (FakeDelayer.LOGGING)
          println(s"[${System.currentTimeMillis()}] $reflectionHost to $mirrorHost request Connect")
        val waitUntil = System.currentTimeMillis() + fakeDelay.toMillis
        while (waitUntil < System.currentTimeMillis()) Thread.sleep(1)
        if (FakeDelayer.LOGGING) println(s"[${System.currentTimeMillis()}] $mirrorHost executing request Connect")
      }

      // simple initialization data for transfer in the other direction
      val (mirrorInitValues, mirrorMaybeFirstFrame) = ReactiveMirror(
        reactive,
        FullMVTurnLocalClone.withPredecessorReplication(connectTurn, mirrorHost, fakeDelay),
        reflectionIsTransient,
        rename.derive("Mirror")
      )(toPulse, reflectionProxy)

      if (fakeDelay != Duration.Zero) {
        if (FakeDelayer.LOGGING)
          println(s"[${System.currentTimeMillis()}] $mirrorHost to $reflectionHost reply Initialize")
        val waitUntil = System.currentTimeMillis() + fakeDelay.toMillis
        while (waitUntil < System.currentTimeMillis()) Thread.sleep(1)
        if (FakeDelayer.LOGGING) println(s"[${System.currentTimeMillis()}] $reflectionHost receive reply Initialize")
      }
      val reflectionInitValues = mirrorInitValues.map {
        case (mirrorTurn, value) =>
          FullMVTurnLocalClone.withPredecessorReplication(mirrorTurn, reflectionHost, fakeDelay) -> value
      }
      val reflectionMaybeFirstFrame = mirrorMaybeFirstFrame.map((turn: FullMVTurn) =>
        FullMVTurnLocalClone.withPredecessorReplication(turn, reflectionHost, fakeDelay)
      )

      reflection.state.retrofitSinkFrames(reflectionInitValues.map(_._1), reflectionMaybeFirstFrame, +1).foreach(
        _.activeBranchDifferential(TurnPhase.Executing, 1)
      )
      for ((reflectionTurn, value) <- reflectionInitValues) reflection.buffer(reflectionTurn, value)
    }
  }
}
