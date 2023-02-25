package rescala.extra.incremental

import rescala.operator.Interface

object IncrementalApi extends Interface.FromScheduler(rescala.interfaces.synchron.scheduler) with IncrementalBundle {
  final type SeqSource[A] = IncSeq[A]

  object SeqSource {
    def apply[A]: SeqSource[A] = empty[A]

    def empty[A]: SeqSource[A] = IncSeq.empty[A](scheduler)
  }

}
