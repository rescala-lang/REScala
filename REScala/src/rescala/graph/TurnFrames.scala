package rescala.graph

import rescala.turns.Turn

/**
 * @author moritzlichter
 */

class ReactiveFrame[T <: ReactiveFrame[T]](
  _turn: Turn,
  r: Reactive,
  _level: Buffer[Int],
  _outgoing: Buffer[Set[Reactive]],
  _incoming: Set[Reactive])
  extends TurnFrame[T](_turn) {
  self: T =>

  private[rescala] val level: Buffer[Int] = _level

  private[rescala] val outgoing: Buffer[Set[Reactive]] = _outgoing

  protected[rescala] var incoming: Set[Reactive] = _incoming

}

class ReactiveFrameImpl (
  turn: Turn,
  r: Reactive,
  level: Buffer[Int],
  outgoing: Buffer[Set[Reactive]],
  incoming: Set[Reactive])
  extends ReactiveFrame[ReactiveFrameImpl](turn, r, level, outgoing, incoming) {
  
  def this(_turn: Turn, r: Reactive, _incoming: Set[Reactive]) {
    this(_turn, r, r.engine.buffer(0, math.max, r.lock), r.engine.buffer(Set(), Buffer.commitAsIs, r.lock), _incoming)
  }
  
}

class PulsingFrame[T <: PulsingFrame[T, P], P](
  _turn: Turn,
  p: Pulsing[P],
  _level: Buffer[Int],
  _outgoing: Buffer[Set[Reactive]],
  _incoming: Set[Reactive],
  _pulses: Buffer[Pulse[P]])
  extends ReactiveFrame[T](_turn, p, _level, _outgoing, _incoming) {
  self: T =>

  protected[rescala] val pulses: Buffer[Pulse[P]] = _pulses
}

class PulsingFrameImpl[P]  (
  turn: Turn,
  p: Pulsing[P],
  level: Buffer[Int],
  outgoing: Buffer[Set[Reactive]],
  incoming: Set[Reactive],
  pulses: Buffer[Pulse[P]])
  extends PulsingFrame[PulsingFrameImpl[P], P](turn, p, level, outgoing, incoming, pulses) {
  
  def this(_turn: Turn, p: Pulsing[P], _incoming: Set[Reactive]) {
    this(_turn, p, p.engine.buffer(0, math.max, p.lock), p.engine.buffer(Set(), Buffer.commitAsIs, p.lock), _incoming, p.engine.buffer(Pulse.none, Buffer.transactionLocal, p.lock))
  }
  
} 

class StatefulFrame[T <: StatefulFrame[T, A], A](
  _turn: Turn,
  p: Stateful[A],
  _level: Buffer[Int],
  _outgoing: Buffer[Set[Reactive]],
  _incoming: Set[Reactive],
  _pulses: Buffer[Pulse[A]])
  extends PulsingFrame[T, A](_turn, p, _level, _outgoing, _incoming, _pulses) {

  self: T =>
  

  pulses.initStrategy(Buffer.keepPulse)

  override def toString = super.toString() + s"[turn=$turn, written=$isWritten, pulses=${pulses.get(null)}]"
}

class StatefulFrameImpl[A] (
  turn: Turn,
  p: Stateful[A],
  level: Buffer[Int],
  outgoing: Buffer[Set[Reactive]],
  incoming: Set[Reactive],
  pulses: Buffer[Pulse[A]])
  extends StatefulFrame[StatefulFrameImpl[A], A] (turn, p, level, outgoing, incoming, pulses) {
  
  def this(_turn: Turn, p: Stateful[A], _incoming: Set[Reactive]) {
    this(_turn, p, p.engine.buffer(0, math.max, p.lock), p.engine.buffer(Set(), Buffer.commitAsIs, p.lock), _incoming, p.engine.buffer(Pulse.none, Buffer.transactionLocal, p.lock))
  }
  
}