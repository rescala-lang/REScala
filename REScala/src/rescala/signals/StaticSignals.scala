package rescala.signals

import rescala._

/**
 * Create a StaticVar
 */
object StaticVar {
  @deprecated("use VarSynt instead", since = "unknown")
  def apply[T](initialValue: T) = new VarSynt(initialValue)
}

/** A dependent reactive value which has static dependencies */
class StaticSignal[+T](reactivesDependsOn: List[DepHolder])(expr: => T) extends DependentSignalImplementation[T] {

  setDependOn(reactivesDependsOn)
  override def initialValue(): T = expr
  override def calculateNewValue(): T = expr
}

/**
 * Create a StaticSignal
 */
object StaticSignal {

  def apply[T](reactivesDependsOn: List[DepHolder])(expr: => T) =
    new StaticSignal(reactivesDependsOn)(expr)

  def apply[T]()(expr: => T): DependentSignal[T] = apply(List())(expr)

  def apply[T](dependencyHolders: DepHolder*)(expr: => T): DependentSignal[T] = apply(dependencyHolders.toList)(expr)
}
