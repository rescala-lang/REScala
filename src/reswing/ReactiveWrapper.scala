package reswing

import scala.events.behaviour.Signal
import scala.events.behaviour.Var

/**
 * Helper class to wrap a Signal-enabled interface around the getter/setter
 * method pairs used in Swing (setter and init constructor arguments).
 * 
 * A signal can be set which will cause the Swing setter method to be called
 * each time the signal changes (setting signal property).
 * Another signal is provided whose value changes according to the value
 * retrieved from the Swing getter method (getting signal property).
 * The object using an instance of this class to wrap a Swing getter/setter
 * is responsible to inform this instance about value changes that occur
 * when the Swing setter method is called or when the value changes by user interaction.
 * 
 * This way, it is possible to set a property reactively using a signal
 * and to use a property in signal expressions.
 */
protected class ReactiveWrapper[T](setter: T => Unit, init: T) {
  private val valueVar = new Var[T](init)
  private val outSignal = Signal { valueVar() }
  private var inSignal : Signal[T] = null
  
  /**
   * Sets the in signal. The in signal is used to update 
   * the wrapped value by calling the Swing setter.
   */
  def signal_=(signal: Signal[T]) {
    if (inSignal != null)
      inSignal.changed -= setter
    
    inSignal = signal
    if (signal != null) {
      setter(signal.getValue)
      signal.changed += setter
    }
  }
  
  /**
   * Gets the out signal.
   * The value is the same that would be returned by the Swing getter.
   */
  def signal = outSignal
  
  /**
   * Sets the current value of the out signal.
   * Should be updated when the Swing setter method is called or
   * when the value changes by user interaction.
   */
  def value_=(value: T) = valueVar() = value
  
  /**
   * Returns the current value of the out signal.
   */
  def value = valueVar()
}
