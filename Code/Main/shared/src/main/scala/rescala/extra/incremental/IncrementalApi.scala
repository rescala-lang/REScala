package rescala.extra.incremental

import rescala.interface.RescalaInterface

object IncrementalApi extends IncrementalBundle with RescalaInterface with rescala.Schedulers.Synchron {

  final type SeqSource[A] = IncSeq[A]

  object SeqSource {
    def apply[A]: SeqSource[A] = empty[A]

    def empty[A]: SeqSource[A] = IncSeq.empty[A](scheduler)
  }

}
