package rescala.graph

import rescala.turns.Turn

/**
 * @author moritzlichter
 */

class ReactiveFrame(
  r: Reactive,
  _level: Buffer[Int],
  _outgoing: Buffer[Set[Reactive]],
  _incoming: Set[Reactive]) {
  
   def this(r: Reactive, _incoming: Set[Reactive]) {
    this(r, r.engine.buffer(0, math.max, r.lock), r.engine.buffer(Set(), Buffer.commitAsIs, r.lock), _incoming)
  }

  private[rescala] val level: Buffer[Int] = _level

  private[rescala] val outgoing: Buffer[Set[Reactive]] = _outgoing

  protected[rescala] var incoming: Set[Reactive] = _incoming
  
  override def toString() = s"${getClass.getSimpleName}{level=${level.get(null)}}"

}


class PulsingFrame[P](
  p: Pulsing[P],
  _level: Buffer[Int],
  _outgoing: Buffer[Set[Reactive]],
  _incoming: Set[Reactive],
  _pulses: Buffer[Pulse[P]])
  extends ReactiveFrame(p, _level, _outgoing, _incoming) {

  def this(p: Pulsing[P], _incoming: Set[Reactive]) {
    this(p, p.engine.buffer(0, math.max, p.lock), p.engine.buffer(Set(), Buffer.commitAsIs, p.lock), _incoming, p.engine.buffer(Pulse.none, Buffer.transactionLocal, p.lock))
  }
  
  protected[rescala] val pulses: Buffer[Pulse[P]] = _pulses
}


class StatefulFrame[A](
  p: Stateful[A],
  _level: Buffer[Int],
  _outgoing: Buffer[Set[Reactive]],
  _incoming: Set[Reactive],
  _pulses: Buffer[Pulse[A]])
  extends PulsingFrame[A](p, _level, _outgoing, _incoming, _pulses) {

  def this(p: Stateful[A], _incoming: Set[Reactive]) {
    this(p, p.engine.buffer(0, math.max, p.lock), p.engine.buffer(Set(), Buffer.commitAsIs, p.lock), _incoming, p.engine.buffer(Pulse.none, Buffer.transactionLocal, p.lock))
  }
  
  pulses.initStrategy(Buffer.keepPulse)

  override def toString = super.toString() + s"[pulses=${pulses.get(null)}]"
}
