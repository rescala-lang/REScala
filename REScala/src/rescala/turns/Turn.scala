package rescala.turns

import rescala.graph.{BufferFactory, Committable, Reactive}

/**
 * The engine that schedules the (glitch-free) evaluation
 * of the nodes in the dependency graph.
 */
trait Turn {

  /** returns the engine of this turn */
  def bufferFactory: BufferFactory

  /** allow turn to handle dynamic access to reactives */
  def accessDynamic(dependency: Reactive): Unit

  /** admits a new source change */
  def admit(reactive: Reactive): Unit

  /** removes a reactive from evaluation */
  def forget(reactive: Reactive): Unit

  /** called when a new reactive is created and registered into the network
    * subclasses are expected to register the reactive with its dependencies
    * and calculate the correct level */
  def create[T <: Reactive](dependencies: Set[Reactive], dynamic: Boolean = false)(f: => T): T

  /** adds a dependency */
  def register(sink: Reactive)(source: Reactive): Unit

  /** removes a dependency */
  def unregister(sink: Reactive)(source: Reactive): Unit

  /** install a new commit handler */
  def schedule(committable: Committable): Unit

  /** plan a new after commit handler. this still runs before releasing locks */
  def observe(f: => Unit): Unit
}
