package rescala.signals

import rescala.propagation.{MaybeTurn, Reactive, Turn}

/**
 * old static signal api
 */
object StaticSignal {
  def apply[T](dependencies: Reactive*)(expr: => T)(implicit maybe: MaybeTurn): Signal[T] = Signals.mapping(dependencies.toSet)(_ => expr)
}
