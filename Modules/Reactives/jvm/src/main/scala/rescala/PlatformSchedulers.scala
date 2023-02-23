package rescala

import rescala.core.Scheduler
import rescala.fullmv.FullMVUtil
import rescala.interface.RescalaInterface
import rescala.parrp.{Backoff, ParRP}

trait PlatformSchedulers {

  object parrp extends interface.RescalaInterface {
    val bundle = new ParRP {}
    override type State[V] = bundle.State[V]
    override val scheduler: Scheduler[State] = bundle.parrpWithBackoff(() => new Backoff())
  }

  val fullmv: FullMVUtil.default.type = FullMVUtil.default

  def byName(name: String): RescalaInterface =
    name match {
      case "parrp"  => parrp
      case "fullmv" => fullmv
      case other    => throw new IllegalArgumentException(s"unknown engine $other")
    }

}
