package reactives.fullmv

import reactives.compat.SignalCompatBundle
import reactives.fullmv.mirrors.localcloning.{FullMVTurnLocalCloneBundle, ReactiveLocalCloneBundle}
import reactives.fullmv.mirrors.{FullMVTurnReflectionBundle, Mirror, ReactiveMirrorBundle, ReactiveReflectionBundle}
import reactives.fullmv.sgt.synchronization.SubsumableLockBundle
import reactives.fullmv.tasks.TaskBundle
import reactives.fullmv.transmitter.ReactiveTransmittableBundle
import reactives.operator.Interface
import reactives.operator.{DefaultImplementations, EventBundle, ObserveBundle, SignalBundle, Sources}

import scala.concurrent.duration.Duration
import scala.util.DynamicVariable

object DistributedFullMVApi extends FullMVBundle with FullMVTurnLocalCloneBundle with Mirror with TurnImplBundle
    with TaskBundle with FullMvStateBundle with SubsumableLockBundle with FullMVTurnReflectionBundle
    with ReactiveLocalCloneBundle with Interface with SignalCompatBundle with EventBundle with SignalBundle
    with Sources with DefaultImplementations with ObserveBundle
    with ReactiveReflectionBundle with ReactiveMirrorBundle with ReactiveTransmittableBundle {

  val scopedScheduler: DynamicVariable[FullMVEngine] = new DynamicVariable(new FullMVEngine(
    Duration.Inf,
    "I dedicate this scheduler to Manuel and Edlira, may they rest independent classes."
  ))

  override def scheduler: FullMVEngine = scopedScheduler.value
}
