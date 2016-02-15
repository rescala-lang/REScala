package rescala.propagation

import rescala.graph.{Committable, Reactive, Spores}

/**
 * The engine that schedules the (glitch-free) evaluation
 * of the nodes in the dependency graph.
 */
trait Turn[S <: Spores] {

  /** used to create state containers of each reactive */
  def bufferFactory: S

  /** allow turn to handle dynamic access to reactives */
  def dependencyInteraction(reactive: Reactive[S]): Unit

  /** admits a new source change */
  def admit(reactive: Reactive[S]): Unit

  /** removes a reactive from evaluation */
  def forget(reactive: Reactive[S]): Unit

  /** called when a new reactive is created and registered into the network
    * subclasses are expected to register the reactive with its dependencies
    * and calculate the correct level */
  def create[T <: Reactive[S]](dependencies: Set[Reactive[S]], dynamic: Boolean = false)(f: => T): T

  /** adds a dependency */
  def discover(sink: Reactive[S])(source: Reactive[S]): Unit

  /** removes a dependency */
  def drop(sink: Reactive[S])(source: Reactive[S]): Unit

  /** install a new commit handler */
  def schedule(committable: Committable): Unit

  /** plan a new after commit handler. this still runs before releasing locks */
  def observe(f: => Unit): Unit

  /** collects and returns dynamic dependencies during the execution of f */
  def collectDependencies[T](f: => T): (T, Set[Reactive[S]])

  /** marks a dependency as used dynamically so it is returned by the innermost call of `collectDependencies` */
  def useDependency(dependency: Reactive[S]): Unit
}
