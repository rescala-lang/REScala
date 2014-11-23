package rescala.propagation.turns.creation

import rescala.propagation.turns.instances.{UnSynchronized, Synchronized, Pessimistic}
import rescala.propagation.turns.Turn

trait TurnFactory {
  /** creates runs and commits a new turn */
  def newTurn[T](f: Turn => T): T

  /** uses the current turn if any or creates a new turn if none*/
  def maybeDynamicTurn[T](f: Turn => T): T
}



