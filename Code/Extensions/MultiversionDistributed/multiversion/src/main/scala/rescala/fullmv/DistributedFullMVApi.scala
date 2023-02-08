package rescala.fullmv

import rescala.compat.SignalCompatBundle
import rescala.fullmv.mirrors.localcloning.{FullMVTurnLocalCloneBundle, ReactiveLocalCloneBundle}
import rescala.fullmv.mirrors.{FullMVTurnReflectionBundle, Mirror, ReactiveMirrorBundle, ReactiveReflectionBundle}
import rescala.fullmv.sgt.synchronization.SubsumableLockBundle
import rescala.fullmv.tasks.TaskBundle
import rescala.fullmv.transmitter.ReactiveTransmittableBundle
import rescala.interface.RescalaInterface
import rescala.operator.{DefaultImplementations, EventBundle, ObserveBundle, SignalBundle, Sources}

import scala.concurrent.duration.Duration
import scala.util.DynamicVariable

object DistributedFullMVApi extends FullMVBundle with FullMVTurnLocalCloneBundle with Mirror with TurnImplBundle
    with TaskBundle with FullMvStateBundle with SubsumableLockBundle with FullMVTurnReflectionBundle
    with ReactiveLocalCloneBundle with RescalaInterface with SignalCompatBundle with EventBundle with SignalBundle
    with Sources with DefaultImplementations with ObserveBundle
    with ReactiveReflectionBundle with ReactiveMirrorBundle with ReactiveTransmittableBundle {

  val scopedScheduler: DynamicVariable[FullMVEngine] = new DynamicVariable(new FullMVEngine(
    Duration.Inf,
    "I dedicate this scheduler to Manuel and Edlira, may they rest independent classes."
  ))

  override def scheduler: FullMVEngine = scopedScheduler.value
}
