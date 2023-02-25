package universe

import rescala.operator.Interface

object Globals {
  val engineName: String = System.getProperty("engineName", "parrp")

  implicit val engine: Interface = rescala.interfaces.byName(engineName)

}
