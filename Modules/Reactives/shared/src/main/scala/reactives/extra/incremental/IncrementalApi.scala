package reactives.extra.incremental

import reactives.operator.Interface

object IncrementalApi extends Interface.FromScheduler(reactives.interfaces.synchron.scheduler) {
  final type SeqSource[A] = IncSeq[A]

  object SeqSource {
    def apply[A]: SeqSource[A] = empty[A]

    def empty[A]: SeqSource[A] = IncSeq.empty[A](scheduler)
  }

}
