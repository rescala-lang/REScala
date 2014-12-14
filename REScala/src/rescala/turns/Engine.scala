package rescala.turns

import rescala.graph.Reactive

import scala.annotation.implicitNotFound

@implicitNotFound(msg = "could not finde a propagation engine, select one from Engines")
trait Engine[+TI <: Turn] {
  /** creates runs and commits a new turn */
  def plan[T](initialWrites: Reactive*)(admissionPhase: TI => T): T

  /** uses the current turn if any or creates a new turn if none */
  def subplan[T](initialWrites: Reactive*)(admissionPhase: TI => T): T
}

