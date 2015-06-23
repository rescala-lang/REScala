package rescala.turns

import rescala.graph.{State, Reactive}

import scala.annotation.implicitNotFound

@implicitNotFound(msg = "could not find a propagation engine, select one from Engines")
trait Engine[S <: State, +TTurn <: Turn[S]] {
  /** used for the creation of state inside reactives */
  private[rescala] def bufferFactory: S

  /** creates runs and commits a new turn */
  def plan[R](initialWrites: Reactive[S]*)(admissionPhase: TTurn => R): R

  /** uses the current turn if any or creates a new turn if none */
  def subplan[T](initialWrites: Reactive[S]*)(admissionPhase: TTurn => T): T
}
