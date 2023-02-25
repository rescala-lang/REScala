package universe

import rescala.operator.Interface
import rescala.scheduler.Schedulers

object Globals {
  val engineName: String = System.getProperty("engineName", "parrp")

  implicit val engine: Interface = Schedulers.byName(engineName)

}
