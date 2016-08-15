package rescala.propagation

import rescala.graph.{Reactive, Struct}

/**
  * Abstract propagation definition that defines phases for reactive propagation through dependent reactive elements.
  *
  * @tparam S Struct type that defines the spore type used to manage the reactive evaluation
  */
trait AbstractPropagation[S <: Struct] extends Turn[S] {

  /**
    * Locks (and potentially otherwise prepares) all affected reactive values to prevent interfering changes.
    *
    * @param initialWrites List of affected reactive values
    */
  def preparationPhase(initialWrites: List[Reactive[S]]): Unit

  /**
    * Performs the actual propagation, setting the new (not yet committed) values for each reactive element.
    */
  def propagationPhase(): Unit

  /**
    * Commits all uncommitted changes to the reactive element.
    */
  def commitPhase(): Unit

  /**
    * Reverts all uncommitted changes to the reactive element.
    */
  def rollbackPhase(): Unit

  /**
    * Call all registered after-commit obverser functions.
    */
  def observerPhase(): Unit

  /**
    * Unlocks (and potentially otherwise reverts the propagation preparations for) each reactive value to allow future
    * turns to run on them.
    */
  def releasePhase(): Unit


  private var collectedDependencies: List[Reactive[S]] = Nil

  def collectMarkedDependencies[T](f: => T): (T, Set[Reactive[S]]) = {
    val old = collectedDependencies
    collectedDependencies = Nil
    // useDependency is called as a side effect of `f` adding new dependencies to collectedDependencies
    val sideEffectingEvaluationResult = f
    val newDependencies = collectedDependencies.toSet
    collectedDependencies = old
    (sideEffectingEvaluationResult, newDependencies)
  }
  def markDependencyAsUsed(dependency: Reactive[S]): Unit = collectedDependencies ::= dependency
}
