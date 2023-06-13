package rescala

import rescala.operator.Interface
import rescala.scheduler.{LevelbasedVariants, SynchronizedSidup, TopbundleImpl}

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
