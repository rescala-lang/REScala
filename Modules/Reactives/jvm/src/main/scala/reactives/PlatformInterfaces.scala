package reactives

import reactives.fullmv.FullMVUtil
import reactives.operator.Interface
import reactives.parrp.ParRPDefault

trait PlatformInterfaces {

//  val parrp: Interface = Interface.from(ParRPDefault.scheduler)

//  val fullmv: FullMVUtil.default.type = FullMVUtil.default

  def byName(name: String): Interface =
    name match {
//      case "parrp"  => parrp
//      case "fullmv" => fullmv
      case other    => throw new IllegalArgumentException(s"unknown engine $other")
    }

//  def defaultPlatformScheduler: parrp.scheduler.type = parrp.scheduler
}
