package rescala.turns

import scala.annotation.implicitNotFound

@implicitNotFound(msg = "could not finde a propagation engine, select one from Engines")
trait Engine {
  /** creates runs and commits a new turn */
  def plan[T](f: Turn => T): T

  /** uses the current turn if any or creates a new turn if none */
  def subplan[T](f: Turn => T): T
}

