package rescala

/**
 * Create a StaticVar
 */
object StaticVar {
  @deprecated("use VarSynt instead", since = "unknown")
  def apply[T](initialValue: T) = new VarSynt(initialValue)
}

/** A dependent reactive value which has static dependencies */
@deprecated("StaticSignal can not handle dynamic level changes, use SignalSynt instead", since = "unknown")
class StaticSignal[+T](reactivesDependsOn: List[DepHolder])(expr: => T) extends DependentSignal[T] {

  var inQueue = false

  private[this] var currentValue = expr

  def get = currentValue

  reactivesDependsOn.foreach(r => {
    if (r.level >= level) level = r.level + 1 // For glitch freedom
    r.addDependent(this) // To be notified in the future
  }) // check
  setDependOn(reactivesDependsOn)

  protected[rescala] def triggerReevaluation() = reEvaluate()

  def reEvaluate(): T = {
    ReactiveEngine.log.nodeEvaluationStarted(this)
    inQueue = false
    val newValue = expr
    if (newValue != currentValue) {
      currentValue = newValue
      timestamps += TS.newTs // Testing
      notifyDependents(currentValue)
    } else {
      ReactiveEngine.log.nodePropagationStopped(this)
      timestamps += TS.newTs // Testing
    }
    ReactiveEngine.log.nodeEvaluationEnded(this)
    newValue
  }

  override def dependsOnchanged(change: Any, dep: DepHolder) = {
    if (!inQueue) {
      inQueue = true
      ReactiveEngine.addToEvalQueue(this)
    }
  }
}

/**
 * Create a StaticSignal
 */
object StaticSignal {

  @deprecated("StaticSignal can not handle dynamic level changes, use SignalSynt instead", since = "unknown")
  def apply[T](reactivesDependsOn: List[DepHolder])(expr: => T) =
    new StaticSignal(reactivesDependsOn)(expr)

  @deprecated("StaticSignal can not handle dynamic level changes, use SignalSynt instead", since = "unknown")
  def apply[T]()(expr: => T): DependentSignal[T] = apply(List())(expr)

  @deprecated("StaticSignal can not handle dynamic level changes, use SignalSynt instead", since = "unknown")
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
