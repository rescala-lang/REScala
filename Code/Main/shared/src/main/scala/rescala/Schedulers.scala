package rescala

import rescala.extra.scheduler.SimpleBundle
import rescala.interface.RescalaInterface
import rescala.scheduler.{Synchron, Unmanaged}

object Schedulers extends PlatformSchedulers {

  object unmanaged extends Unmanaged with RescalaInterface

  object synchron extends Synchron with RescalaInterface

  object simple extends SimpleBundle with RescalaInterface {
    override def scheduler: simple.Scheduler = SimpleScheduler
  }

  override def byName(name: String): RescalaInterface =
    name match {
      case "synchron"  => synchron
      case "unmanaged" => unmanaged
      case "simple"    => simple
      case other       => super.byName(name)
    }

}
