package rescala.turns

import rescala.graph.{Buffer, Reactive, SimpleBuffer}
import rescala.synchronization.TurnLock

import scala.annotation.implicitNotFound

@implicitNotFound(msg = "could not finde a propagation engine, select one from Engines")
trait Engine[+TTurn <: Turn] {
  
  // Created specialized buffer methods, because pipeling needs different buffers per field
  def bufferIncoming[A](default: A, commitStrategy: (A, A) => A, at: Reactive): Buffer[A] = new SimpleBuffer[A](default, commitStrategy, at.lock)
  def bufferOutgoing[A](default: A, commitStrategy: (A, A) => A, at: Reactive): Buffer[A] = new SimpleBuffer[A](default, commitStrategy, at.lock)
  def bufferPulses[A](default: A, commitStrategy: (A, A) => A, at: Reactive): Buffer[A] = new SimpleBuffer[A](default, commitStrategy, at.lock)
  def bufferLevel[A](default: A, commitStrategy: (A, A) => A, at: Reactive): Buffer[A] = new SimpleBuffer[A](default, commitStrategy, at.lock)

  /** creates runs and commits a new turn */
  def plan[R](initialWrites: Reactive*)(admissionPhase: TTurn => R): R

  /** uses the current turn if any or creates a new turn if none */
  def subplan[T](initialWrites: Reactive*)(admissionPhase: TTurn => T): T
}
