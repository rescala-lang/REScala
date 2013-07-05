package reswing

import macro.SignalMacro.{SignalM => Signal}
import react.Signal
import react.SignalSynt
import react.events.ImperativeEvent

/**
 * Provides a signal that is used to update Swing values, but also
 * changes its own value whenever the corresponding Swing value changes.
 * 
 * A signal can be set which will cause the Swing setter method to be called
 * each time the signal changes.
 * The signal this class provides changes its value according to the value
 * retrieved from the Swing getter method.
 * 
 * This way, it is possible to set a property reactively using a signal
 * and to use a property in signal expressions.
 */
class ImperativeSignal[T] private (private[reswing] val inputSignal: Signal[T]) {
  private val event = new ImperativeEvent[T] //#EVT
  private val signal = if (inputSignal != null) //#SIG
      (inputSignal.changed || event) latest inputSignal.getValue //#IF //#IF
    else
      event latest null.asInstanceOf[T] //#IF
  
  private[reswing] def apply(value: T) = event(value)
}

object ImperativeSignal {
  implicit def toSignal[T](signal: ImperativeSignal[T]) = signal.signal
  implicit def fromSignal[T](signal: Signal[T]) = new ImperativeSignal(signal)
  implicit def fromValue[T](value: T) = new ImperativeSignal(Signal{ value }) //#SIG
  
  def noSignal[T] = new ImperativeSignal[T](null)
  def noSignal[T](value: T) = {
    val signal = new ImperativeSignal[T](null)
    signal(value)
    signal
  }
}