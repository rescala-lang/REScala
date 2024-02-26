package reactives

import reactives.operator.Interface
import reactives.scheduler.{LevelbasedVariants, SynchronizedSidup, TopbundleImpl}

object interfaces extends PlatformInterfaces {

  val unmanaged: Interface = Interface.from(LevelbasedVariants.unmanaged)

  val synchron: Interface = Interface.from(LevelbasedVariants.synchron)

  val toposort: Interface = Interface.from(TopbundleImpl.TopoScheduler)

  val sidup: Interface = Interface.from(SynchronizedSidup.scheduler)

  override def byName(name: String): Interface =
    name match {
      case "synchron"  => synchron
      case "unmanaged" => unmanaged
      case "toposort"  => toposort
      case "sidup"     => sidup
      case other       => super.byName(name)
    }
}
