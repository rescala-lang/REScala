package rescala.turns

import rescala.graph.{SynchronizationFactory, Reactive}

import scala.annotation.implicitNotFound

@implicitNotFound(msg = "could not finde a propagation engine, select one from Engines")
trait Engine[+TTurn <: Turn] {
  /** used for the creation of state inside reactives */
  private[rescala] def bufferFactory: SynchronizationFactory

  /** creates runs and commits a new turn */
  def plan[R](initialWrites: Reactive*)(admissionPhase: TTurn => R): R

  /** uses the current turn if any or creates a new turn if none */
  def subplan[T](initialWrites: Reactive*)(admissionPhase: TTurn => T): T
}
