package reactives.scheduler

import reactives.core.{DynamicScope, Scheduler}

trait GlobalCandidate[S[_]] {
  type State[V] = S[V]
  def scheduler: Scheduler[State]
  def dynamicScope: DynamicScope[State]
}

object GlobalCandidate {

  transparent inline def select: GlobalCandidate[?] =
    inline GeneratedSelection.selection match
      case "toposort" => toposort
      case "calculus" => calculus
      case "sidup"    => sidup
      case "levelled" => levelled
      case "parrp"    => PlatformCandidates.parrp
      case "fullmv"   => PlatformCandidates.fullmv
      case other      => levelled

  // the type of selected might be expressible with a match type,
  // but type inference will take care of that for us instead
  val selected = select

  object levelled extends GlobalCandidate[LevelbasedVariants.LevelState] {
    override def scheduler: Scheduler[State]       = LevelbasedVariants.synchron
    override def dynamicScope: DynamicScope[State] = LevelbasedVariants.synchron.dynamicScope
  }

  object calculus extends GlobalCandidate[CalculusLike.StoreValue] {
    override def scheduler: Scheduler[State]       = CalculusLike.FScheduler
    override def dynamicScope: DynamicScope[State] = CalculusLike.FScheduler.dynamicScope
  }

  object toposort extends GlobalCandidate[TopoBundle.TopoState] {
    override def scheduler: Scheduler[State]       = TopoBundle.TopoScheduler
    override def dynamicScope: DynamicScope[State] = TopoBundle.TopoScheduler.dynamicScope
  }

  object sidup extends GlobalCandidate[SynchronizedSidup.SidupState] {
    override def scheduler: Scheduler[State]       = SynchronizedSidup.scheduler
    override def dynamicScope: DynamicScope[State] = SynchronizedSidup.scheduler.dynamicScope
  }

}
