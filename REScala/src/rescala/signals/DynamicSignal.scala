package rescala.signals

import rescala.propagation._

/**
 * old DynamicSignal api
 */
object DynamicSignal {
  def apply[T](expr: Turn => T): Signal[T] = Signals.dynamic(Set())(expr)
  def apply[T](dependencies: List[Pulsing[Any]])(expr: Turn => T): Signal[T] = Signals.dynamic(dependencies.toSet)(expr)
  def apply[T](dependencies: Pulsing[Any]*)(expr: Turn => T): Signal[T] = Signals.dynamic(dependencies.toSet)(expr)
}
