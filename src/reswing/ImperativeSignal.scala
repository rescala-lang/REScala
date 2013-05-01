package reswing

import scala.events.ImperativeEvent
import scala.events.behaviour.Signal

class ImperativeSignal[T] private (private[reswing] val inputSignal: Signal[T]) {
  private val event = new ImperativeEvent[T]
  private val signal = if (inputSignal != null)
      (inputSignal.changed || event) latest inputSignal.getValue
    else
      event latest null.asInstanceOf[T]
  
  private[reswing] def apply(value: T) = event(value)
}

object ImperativeSignal {
  implicit def toSignal[T](signal: ImperativeSignal[T]) = signal.signal
  implicit def fromSignal[T](signal: Signal[T]) = new ImperativeSignal(signal)
  implicit def fromValue[T](value: T) = new ImperativeSignal(Signal{ value })
  
  def noSignal[T] = new ImperativeSignal[T](null)
  def noSignal[T](value: T) = {
    val signal = new ImperativeSignal[T](null)
    signal(value)
    signal
  }
}