package rescala.propagation

import rescala.graph.{Reactive, Struct}

trait AbstractPropagation[S <: Struct] extends Turn[S] {

  def preparationPhase(initialWrites: List[Reactive[S]]): Unit

  def propagationPhase(): Unit

  def commitPhase(): Unit

  def rollbackPhase(): Unit

  def observerPhase(): Unit

  def releasePhase(): Unit


  private var collectedDependencies: List[Reactive[S]] = Nil

  def collectDependencies[T](f: => T): (T, Set[Reactive[S]]) = {
    val old = collectedDependencies
    collectedDependencies = Nil
    // useDependency is called as a side effect of `f` adding new dependencies to collectedDependencies
    val sideEffectingEvaluationResult = f
    val newDependencies = collectedDependencies.toSet
    collectedDependencies = old
    (sideEffectingEvaluationResult, newDependencies)
  }
  def useDependency(dependency: Reactive[S]): Unit = collectedDependencies ::= dependency
}
