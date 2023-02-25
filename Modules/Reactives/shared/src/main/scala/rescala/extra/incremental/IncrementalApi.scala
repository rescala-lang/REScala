package rescala.extra.incremental

import rescala.operator.Interface
import rescala.scheduler.Schedulers

object IncrementalApi extends Interface.FromScheduler(Schedulers.synchron.scheduler) with IncrementalBundle {
  final type SeqSource[A] = IncSeq[A]

  object SeqSource {
    def apply[A]: SeqSource[A] = empty[A]

    def empty[A]: SeqSource[A] = IncSeq.empty[A](scheduler)
  }

}
