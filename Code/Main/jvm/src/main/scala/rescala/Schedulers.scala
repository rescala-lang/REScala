package rescala

import rescala.extra.scheduler.SimpleBundle
import rescala.interface.RescalaInterface
import rescala.parrp.{Backoff, ParRP}
import rescala.scheduler.{Synchron, Unmanaged}

object Schedulers {

  object parrp extends interface.RescalaInterface with ParRP {
    override val scheduler: Scheduler = parrpWithBackoff(() => new Backoff())
  }

  object unmanaged extends Unmanaged with RescalaInterface

  object synchron extends Synchron with RescalaInterface

  object simple extends SimpleBundle with RescalaInterface {
    override def scheduler: simple.Scheduler = SimpleScheduler
  }

  def byName(name: String): RescalaInterface =
    name match {
      case "synchron"  => synchron
      case "unmanaged" => unmanaged
      case "parrp"     => parrp
      case "simple"    => simple
      case other       => throw new IllegalArgumentException(s"unknown engine $other")
    }

}
