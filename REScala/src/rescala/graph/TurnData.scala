package rescala.graph

import rescala.synchronization.{TurnLock, Key}
import rescala.turns.Turn

trait TurnData[T <: TurnData[T]] {

  protected[rescala] def turn: Turn
  protected[rescala] def copy() : T
  
  protected[rescala] def isWritten() : Boolean
  
}

trait ReactiveTurnData[T <: ReactiveTurnData[T] ]extends TurnData[T] {

  
  
  private[rescala] def level: Buffer[Int];
  // init engine.buffer(0, math.max, lock)

  private[rescala] def outgoing: Buffer[Set[Reactive[_]]];
  // init engine.buffer(Set(), Buffer.commitAsIs, lock)
  
  protected[rescala] def incoming(implicit turn: Turn): Set[Reactive[_]]
  
}

trait PulsingTurnData[+P,T <: PulsingTurnData[P,T]] extends ReactiveTurnData[T] {
  protected [rescala] def pulses: Buffer[Pulse[P]];
    // init engine.buffer(Pulse.none, Buffer.transactionLocal, lock)
}