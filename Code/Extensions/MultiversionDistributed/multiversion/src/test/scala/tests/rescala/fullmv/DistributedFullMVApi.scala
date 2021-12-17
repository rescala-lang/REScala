package tests.rescala.fullmv

import rescala.compat.SignalCompatBundle
import rescala.core.Core
import rescala.fullmv.mirrors.{FullMVTurnReflectionBundle, Mirror, ReactiveMirrorBundle, ReactiveReflectionBundle}
import rescala.fullmv.mirrors.localcloning.{FullMVTurnLocalCloneBundle, ReactiveLocalCloneBundle}
import rescala.fullmv.{FullMVBundle, FullMvStateBundle, TurnImplBundle}
import rescala.fullmv.sgt.synchronization.SubsumableLockBundle
import rescala.fullmv.tasks.TaskBundle
import rescala.fullmv.transmitter.ReactiveTransmittableBundle
import rescala.interface.RescalaInterface
import rescala.operator.{DefaultImplementations, EventBundle, Observing, SignalBundle, Sources}

import scala.concurrent.duration.Duration

object DistributedFullMVApi extends FullMVBundle with FullMVTurnLocalCloneBundle with Mirror with TurnImplBundle
    with TaskBundle with FullMvStateBundle with SubsumableLockBundle with FullMVTurnReflectionBundle
    with ReactiveLocalCloneBundle with RescalaInterface with SignalCompatBundle with EventBundle with SignalBundle
    with Sources with DefaultImplementations with Observing with Core
    with ReactiveReflectionBundle with ReactiveMirrorBundle with ReactiveTransmittableBundle {
  override def scheduler: FullMVEngine = new FullMVEngine(
    Duration.Inf,
    "I dedicate this scheduler to Manuel and Edlira, may they rest independent classes."
  )
}
