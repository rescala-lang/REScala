package universe

import rescala.Schedulers
import rescala.interface.RescalaInterface

object Globals {
  val engineName: String = System.getProperty("engineName", "parrp")

  implicit val engine: RescalaInterface = Schedulers.byName(engineName)

}
