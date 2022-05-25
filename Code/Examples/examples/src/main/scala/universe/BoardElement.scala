package universe

import universe.Globals.engine._

import scala.annotation.nowarn

abstract class BoardElement(implicit val world: World) {

  lazy val dies: Event[Unit] = isDead changedTo true

  /** A signal denoting if this element is dead ( = should be removed from the board) */
  val isDead: Signal[Boolean]
  def isAnimal: Boolean

  /** Some imperative code that is called each tick */
  def doStep(@nowarn pos: Pos): Unit = {}
}
