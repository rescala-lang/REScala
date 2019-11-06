package rescala

import rescala.core.Scheduler
import rescala.parrp.ParRP

/**
  * @author gerizuna
  * @since 07.07.19
  */
object collectionsDefault extends interface.RescalaInterface[ParRP] {
  final type SeqSource[A] = incremental.IncSeq[A, ParRP]

  override def scheduler: Scheduler[ParRP] = rescala.Schedulers.parrp

  object SeqSource {
    def apply[A]: SeqSource[A] = empty[A]

    def empty[A]: SeqSource[A] = incremental.IncSeq.empty[A, ParRP](scheduler)
  }

}
