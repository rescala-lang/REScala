package rescala.turns

import rescala.graph.{Buffer, Reactive, SimpleBuffer}
import rescala.synchronization.TurnLock

import scala.annotation.implicitNotFound

@implicitNotFound(msg = "could not finde a propagation engine, select one from Engines")
trait Engine[+TTurn <: Turn] {
  def buffer[A](default: A, commitStrategy: (A, A) => A, at: Reactive, takePrevious : Boolean = false): Buffer[A] = new SimpleBuffer[A](default, commitStrategy, at.lock)

  /** creates runs and commits a new turn */
  def plan[R](initialWrites: Reactive*)(admissionPhase: TTurn => R): R

  /** uses the current turn if any or creates a new turn if none */
  def subplan[T](initialWrites: Reactive*)(admissionPhase: TTurn => T): T
}
