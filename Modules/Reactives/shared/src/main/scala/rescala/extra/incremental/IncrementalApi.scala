package rescala.extra.incremental

import rescala.operator.Interface
import rescala.scheduler.Schedulers

object IncrementalApi extends IncrementalBundle with Interface with Schedulers.Synchron {
  override type BundleState[V] = State[V]
  override type ReSource  = rescala.core.ReSource.of[State]
  final type SeqSource[A] = IncSeq[A]

  object SeqSource {
    def apply[A]: SeqSource[A] = empty[A]

    def empty[A]: SeqSource[A] = IncSeq.empty[A](scheduler)
  }

}
