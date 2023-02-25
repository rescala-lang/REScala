package rescala.scheduler

import rescala.core.Scheduler
import rescala.fullmv.FullMVUtil
import rescala.operator.Interface
import rescala.parrp.{Backoff, ParRP}

trait PlatformSchedulers {

  object parrp extends Interface with ParRP {
    override val scheduler: Scheduler[State] = parrpWithBackoff(() => new Backoff())
  }

  val fullmv: FullMVUtil.default.type = FullMVUtil.default

  def byName(name: String): Interface =
    name match {
      case "parrp"  => parrp
      case "fullmv" => fullmv
      case other    => throw new IllegalArgumentException(s"unknown engine $other")
    }


  type defaultPlatformState[V] = parrp.State[V]
  def defaultPlatformScheduler: parrp.scheduler.type = parrp.scheduler
}
