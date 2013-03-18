package reswing

import scala.events.behaviour.Signal
import scala.events.behaviour.Var

// TODO: not sure I get the sense of this class, can you add some comments ?
protected class ReactiveWrapper[T](setter: T => Unit, init: T) {
  private val valueVar = new Var[T](init)
  private val outSignal = Signal { valueVar() }
  private var inSignal : Signal[T] = null
  
  def signal_=(signal: Signal[T]) {
    if (inSignal != null)
      inSignal.changed -= setter
    
    inSignal = signal
    if (signal != null) {
      setter(signal.getValue)
      signal.changed += setter
    }
  }
  
  def signal = outSignal
  
  def value_=(value: T) = valueVar() = value
  
  def value = valueVar()
}
