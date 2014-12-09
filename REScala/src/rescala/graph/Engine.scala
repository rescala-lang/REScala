package rescala.graph

import rescala.turns.Turn

import scala.annotation.implicitNotFound

@implicitNotFound(msg = "could not finde a propagation engine, select one from Engines")
trait Engine {
  /** creates runs and commits a new turn */
  def startNew[T](f: Turn => T): T

  /** uses the current turn if any or creates a new turn if none */
  def startKeep[T](f: Turn => T): T
}

