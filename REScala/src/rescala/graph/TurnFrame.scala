package rescala.graph

import rescala.synchronization.{TurnLock}
import rescala.turns.Turn

class TurnFrame(_turn: Turn) {
  
  private var written = false;
  private var toRemove = false;
  
  protected[rescala] def turn: Turn = _turn;
  
  protected[rescala] def isWritten() : Boolean = written
  protected[rescala] def markWritten() = written = true
  protected[rescala] def shouldBeRemoved() = toRemove
  protected[rescala] def markToBeRemoved() = toRemove = true
  
  
  
}

class ReactiveFrame(
    _turn : Turn, 
    r : Reactive, 
    _level : Buffer[Int] , 
    _outgoing: Buffer[Set[Reactive]],
    _incoming: Set[Reactive]) 
  extends TurnFrame(_turn) {
 
  def this (_turn: Turn, r:Reactive, _incoming:Set[Reactive]) {
    this(_turn, r, r.engine.buffer(0, math.max, r.lock), r.engine.buffer(Set(), Buffer.commitAsIs, r.lock), _incoming)
  }

 
  private[rescala] val level: Buffer[Int] = _level

  private[rescala] val outgoing: Buffer[Set[Reactive]] = _outgoing
  
  protected[rescala] var incoming: Set[Reactive] = _incoming
  
}

class PulsingFrame[P](
    _turn : Turn, 
    p : Pulsing[P], 
    _level : Buffer[Int] , 
    _outgoing: Buffer[Set[Reactive]],
    _incoming: Set[Reactive],
     _pulses: Buffer[Pulse[P]]) 
  extends ReactiveFrame(_turn, p, _level, _outgoing, _incoming) {
  
  def this (_turn: Turn, p : Pulsing[P], _incoming:Set[Reactive]) {
    this(_turn, p, p.engine.buffer(0, math.max, p.lock), p.engine.buffer(Set(), Buffer.commitAsIs, p.lock), _incoming,p.engine.buffer(Pulse.none, Buffer.transactionLocal, p.lock))
  }
  
   protected [rescala] val pulses : Buffer[Pulse[P]] = _pulses
}

class StatefulFrame[A](
    _turn : Turn, 
    p : Stateful[A], 
    _level : Buffer[Int] , 
    _outgoing: Buffer[Set[Reactive]],
    _incoming: Set[Reactive],
    _pulses: Buffer[Pulse[A]])
  extends PulsingFrame[A](_turn, p, _level, _outgoing, _incoming, _pulses) {
 
  def this (_turn: Turn, p : Stateful[A], _incoming:Set[Reactive]) {
    this(_turn, p, p.engine.buffer(0, math.max, p.lock), p.engine.buffer(Set(), Buffer.commitAsIs, p.lock), _incoming,p.engine.buffer(Pulse.none, Buffer.transactionLocal, p.lock))
  }
  
  pulses.initStrategy(Buffer.keepPulse)
}