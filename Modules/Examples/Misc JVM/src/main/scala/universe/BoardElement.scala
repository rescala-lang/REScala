package universe

import reactives.default.*

abstract class BoardElement(using World) {
  
  val world: World = summon[World]

  lazy val dies: Event[Any] = isDead.changed.filter(_ == true)

  /** A signal denoting if this element is dead ( = should be removed from the board) */
  val isDead: Signal[Boolean]
  def isAnimal: Boolean

  /** Some imperative code that is called each tick */
  def doStep(pos: Pos): Unit = {}
}
