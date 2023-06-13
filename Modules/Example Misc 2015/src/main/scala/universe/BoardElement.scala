package universe

import universe.Globals.engine._

abstract class BoardElement(implicit val world: World) {

  lazy val dies: Event[Any] = isDead.changed.filter(_ == true)

  /** A signal denoting if this element is dead ( = should be removed from the board) */
  val isDead: Signal[Boolean]
  def isAnimal: Boolean

  /** Some imperative code that is called each tick */
  def doStep(pos: Pos): Unit = {}
}
