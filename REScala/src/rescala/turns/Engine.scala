package rescala.turns

import rescala.graph.{SimpleBuffer, Buffer, Reactive}
import rescala.synchronization.TurnLock

import scala.annotation.implicitNotFound

@implicitNotFound(msg = "could not finde a propagation engine, select one from Engines")
trait Engine[+TI <: Turn] {
  def buffer[A](default: A, commitStrategy: (A, A) => A, writeLock: TurnLock): Buffer[A] = new SimpleBuffer[A](default, commitStrategy, writeLock)

  /** creates runs and commits a new turn */
  def plan[T1, T2](initialWrites: Reactive*)(admissionPhase: TI => T1)(checkPhase: (TI, T1) => T2): T2

  def planned[T1](initialWrites: Reactive*)(admissionPhase: TI => T1): T1 = plan(initialWrites: _*)(admissionPhase)((_, v) => v)

  /** uses the current turn if any or creates a new turn if none */
  def subplan[T](initialWrites: Reactive*)(admissionPhase: TI => T): T
}
