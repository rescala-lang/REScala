package rescala.signals

object Var {
  def apply[T](initval: T) = new VarSynt(initval)
}

/** A root Reactive value without dependencies which can be set */
trait Var[T] extends Signal[T] {
  def set(newValue: T): Unit
  final def update(newValue: T): Unit = set(newValue)
}
