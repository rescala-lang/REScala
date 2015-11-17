package universe


import rescala.Signals
import AEngine.engine
import AEngine.engine._
import scala.collection.immutable.IndexedSeq
import scala.collection.mutable
import scala.collection.mutable.Map
import scala.util.Random

abstract class BoardElement(implicit val world: World) {

  def isAnimal: Boolean

  /** A signal denoting if this element is dead ( = should be removed from the board) */
  val isDead: Signal[Boolean]
  lazy val dies: Event[Unit] = isDead changedTo true

  /** Some imperative code that is called each tick */
  def doStep(pos: Pos): Unit = {}
}
