package universe

import reactives.operator.Interface

object Globals {
  val engineName: String = System.getProperty("engineName", "parrp")

  implicit val engine: Interface = reactives.interfaces.byName(engineName)

}
