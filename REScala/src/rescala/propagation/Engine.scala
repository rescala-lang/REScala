package rescala.propagation

import rescala.propagation.turns.Turn

trait Engine {
  /** creates runs and commits a new turn */
  def startNew[T](f: Turn => T): T

  /** uses the current turn if any or creates a new turn if none */
  def startKeep[T](f: Turn => T): T
}

