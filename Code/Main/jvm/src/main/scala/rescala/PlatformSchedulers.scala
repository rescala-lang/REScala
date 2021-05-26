package rescala

import rescala.fullmv.FullMVEngine
import rescala.interface.RescalaInterface
import rescala.parrp.{Backoff, ParRP}

trait PlatformSchedulers {

  object parrp extends interface.RescalaInterface with ParRP {
    override val scheduler: Scheduler = parrpWithBackoff(() => new Backoff())
  }

  val fullmv: FullMVEngine.default.type = FullMVEngine.default

  def byName(name: String): RescalaInterface =
    name match {
      case "parrp"  => parrp
      case "fullmv" => fullmv
      case other    => throw new IllegalArgumentException(s"unknown engine $other")
    }

}
