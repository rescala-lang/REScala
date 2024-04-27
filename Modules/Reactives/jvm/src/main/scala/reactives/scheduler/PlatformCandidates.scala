package reactives.scheduler

import reactives.core.{DynamicScope, Scheduler}
import reactives.scheduler.GlobalCandidate

object PlatformCandidates {

  transparent inline def furtherSelect(inline selection: String) =
    inline selection match
      case "parrp"  => Some(parrp)
      case "fullmv" => Some(fullmv)
      case other    => None

  object parrp extends GlobalCandidate[reactives.parrp.ParRPDefault.ParRPState] {
    override def scheduler: Scheduler[State]       = reactives.parrp.ParRPDefault.scheduler
    override def dynamicScope: DynamicScope[State] = reactives.parrp.ParRPDefault.scheduler.dynamicScope
  }

  object fullmv extends GlobalCandidate[reactives.fullmv.State] {
    override def scheduler: Scheduler[State]       = reactives.fullmv.FullMVUtil.defaultScheduler
    override def dynamicScope: DynamicScope[State] = reactives.fullmv.FullMVUtil.defaultScheduler.dynamicScope
  }
}
