package rescala.graph

import rescala.synchronization.{TurnLock}
import rescala.turns.Turn

class TurnData(_turn: Option[Turn], _written : Boolean) {

  def this(_turn : Option[Turn]) {
    this(_turn, false)
  }
  
  private var written = _written;
  
  protected[rescala] def turn: Option[Turn] = _turn;
  
  protected[rescala] def isWritten() : Boolean = written
  
  
  
}

class ReactiveTurnData(
    _turn : Option[Turn], 
    r : Reactive, 
    _level : Buffer[Int] , 
    _outgoing: Buffer[Set[Reactive]],
    _incoming: Set[Reactive]) 
  extends TurnData(_turn) {
 
  def this (_turn: Option[Turn], r:Reactive, _incoming:Set[Reactive]) {
    this(_turn, r, r.engine.buffer(0, math.max, r.lock), r.engine.buffer(Set(), Buffer.commitAsIs, r.lock), _incoming)
  }

 
  private[rescala] val level: Buffer[Int] = _level

  private[rescala] val outgoing: Buffer[Set[Reactive]] = _outgoing
  
  protected[rescala] var incoming: Set[Reactive] = _incoming
  
}

class PulsingTurnData[P](
    _turn : Option[Turn], 
    p : Pulsing[P], 
    _level : Buffer[Int] , 
    _outgoing: Buffer[Set[Reactive]],
    _incoming: Set[Reactive],
     _pulses: Buffer[Pulse[P]]) 
  extends ReactiveTurnData(_turn, p, _level, _outgoing, _incoming) {
  
  def this (_turn: Option[Turn], p : Pulsing[P], _incoming:Set[Reactive]) {
    this(_turn, p, p.engine.buffer(0, math.max, p.lock), p.engine.buffer(Set(), Buffer.commitAsIs, p.lock), _incoming,p.engine.buffer(Pulse.none, Buffer.transactionLocal, p.lock))
  }
  
   protected [rescala] val pulses : Buffer[Pulse[P]] = _pulses
}

class StatefulTurnData[A](
    _turn : Option[Turn], 
    p : Stateful[A], 
    _level : Buffer[Int] , 
    _outgoing: Buffer[Set[Reactive]],
    _incoming: Set[Reactive],
    _pulses: Buffer[Pulse[A]])
  extends PulsingTurnData[A](_turn, p, _level, _outgoing, _incoming, _pulses) {
 
  def this (_turn: Option[Turn], p : Stateful[A], _incoming:Set[Reactive]) {
    this(_turn, p, p.engine.buffer(0, math.max, p.lock), p.engine.buffer(Set(), Buffer.commitAsIs, p.lock), _incoming,p.engine.buffer(Pulse.none, Buffer.transactionLocal, p.lock))
  }
  
  pulses.initStrategy(Buffer.keepPulse)
}