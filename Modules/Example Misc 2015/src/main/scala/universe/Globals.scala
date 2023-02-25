package universe

import rescala.Schedulers
import rescala.operator.Interface

object Globals {
  val engineName: String = System.getProperty("engineName", "parrp")

  implicit val engine: Interface = Schedulers.byName(engineName)

}
