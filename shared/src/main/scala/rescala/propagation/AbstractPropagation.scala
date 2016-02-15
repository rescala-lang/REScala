package rescala.propagation

import rescala.graph.{Reactive, Spores}

trait AbstractPropagation[S <: Spores] extends Turn[S] {

  def lockPhase(initialWrites: List[Reactive[S]]): Unit

  def propagationPhase(): Unit

  def commitPhase(): Unit

  def rollbackPhase(): Unit

  def observerPhase(): Unit

  def releasePhase(): Unit


  var collectedDependencies: List[Reactive[S]] = Nil

  def collectDependencies[T](f: => T): (T, Set[Reactive[S]]) = {
    val old = collectedDependencies
    collectedDependencies = Nil
    // useDependency is called as a side effect of `f` adding new dependencies to collectedDependencies
    val res = (f, collectedDependencies.toSet)
    collectedDependencies = old
    res
  }
  def useDependency(dependency: Reactive[S]): Unit = collectedDependencies ::= dependency
}
