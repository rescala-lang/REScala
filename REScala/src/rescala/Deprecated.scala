package rescala

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

/* TODO: Do we really need two types of handlers? Can we use EventHandler? */
object Handler {
  //def apply[T] (exp: => T) = new EventHandler((_: Unit) => exp)
  def apply[T](exp: => T) = new Handler(exp)
}

class Handler[T](exp: => T) extends Dependent {
  override def dependsOnchanged(change: Any, dep: DepHolder) = exp
  def triggerReevaluation() = exp
}
