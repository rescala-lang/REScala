package reactives.core

/** [[ReevTicket]] is given to the [[Derived]] reevaluate method and allows to access other reactives.
  * The ticket tracks return values, such as dependencies, the value, and if the value should be propagated.
  * Such usages make it unsuitable as an API for the user, where [[StaticTicket]] or [[DynamicTicket]] should be used instead.
  */
final class ReevTicket[State[_], V](tx: Transaction[State], private var _before: V, accessHandler: AccessHandler[State])
    extends DynamicTicket(tx)
    with Result[State, V] {

  private var collectedDependencies: Set[ReSource.of[State]] = null

  // dependency tracking accesses
  private[reactives] override def collectStatic(reactive: ReSource.of[State]): reactive.Value = {
    assert(collectedDependencies == null || collectedDependencies.contains(reactive))
    accessHandler.staticAccess(reactive)
  }

  private[reactives] override def collectDynamic(reactive: ReSource.of[State]): reactive.Value = {
    assert(collectedDependencies != null, "may not access dynamic dependencies without tracking dependencies")
    val updatedDeps = collectedDependencies + reactive
    if updatedDeps eq collectedDependencies then {
      accessHandler.staticAccess(reactive)
    } else {
      collectedDependencies = updatedDeps
      accessHandler.dynamicAccess(reactive)
    }
  }

  // inline result into ticket, to reduce the amount of garbage during reevaluation
  private var _propagate          = false
  private var value: V            = scala.compiletime.uninitialized
  private var effect: Observation = null
  override def toString: String =
    s"Result(value = $value, propagate = $activate, deps = $collectedDependencies)"
  def before: V = _before

  /** Advises the ticket to track dynamic dependencies.
    * The passed initial set of dependencies may be processed as if they were static,
    * and are also returned in the resulting dependencies.
    */
  def trackDependencies(initial: Set[ReSource.of[State]]): ReevTicket[State, V] = {
    collectedDependencies = initial; this
  }
  def trackStatic(): ReevTicket[State, V]             = { collectedDependencies = null; this }
  def withPropagate(p: Boolean): ReevTicket[State, V] = { _propagate = p; this }
  def withValue(v: V): ReevTicket[State, V] = {
    require(v != null, "value must not be null")
    value = v
    _propagate = true;
    this
  }
  def withEffect(v: Observation): ReevTicket[State, V] = { effect = v; this }

  override def activate: Boolean                         = _propagate
  override def forValue(f: V => Unit): Unit              = if value != null then f(value)
  override def forEffect(f: Observation => Unit): Unit   = if effect != null then f(effect)
  override def inputs(): Option[Set[ReSource.of[State]]] = Option(collectedDependencies)

  def reset[NT](nb: NT): ReevTicket[State, NT] = {
    _propagate = false
    value = null.asInstanceOf[V]
    effect = null
    collectedDependencies = null
    val res = this.asInstanceOf[ReevTicket[State, NT]]
    res._before = nb
    res
  }
}
