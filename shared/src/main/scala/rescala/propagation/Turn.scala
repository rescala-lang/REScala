package rescala.propagation

import rescala.graph.{Pulse, Buffer, Reactive, Struct}

/**
 * The engine that schedules the (glitch-free) evaluation
 * of the nodes in the dependency graph.
 */
trait Turn[S <: Struct] {
  def pulses[P](budP: S#SporeP[P, Reactive[S]]): Buffer[Pulse[P]]

  def updateIncoming[R](bud: S#Spore[R], newDependencies: Set[R]): Unit

  def incoming[R](bud: S#Spore[R]): Set[R]


  /** used to create state containers of each reactive */
  def bufferFactory: S

  /** allow turn to handle dynamic access to reactives */
  def dependencyInteraction(reactive: Reactive[S]): Unit

  /** called when a new reactive is created and registered into the network
    * subclasses are expected to register the reactive with its dependencies
    * and calculate the correct level */
  def create[T <: Reactive[S]](dependencies: Set[Reactive[S]], dynamic: Boolean = false)(f: => T): T

  /** install a new commit handler */
  def schedule(committable: Committable): Unit

  /** plan a new after commit handler. this still runs before releasing locks */
  def observe(f: => Unit): Unit

  /** collects and returns dynamic dependencies during the execution of f */
  def collectDependencies[T](f: => T): (T, Set[Reactive[S]])

  /** marks a dependency as used dynamically so it is returned by the innermost call of `collectDependencies` */
  def useDependency(dependency: Reactive[S]): Unit
}
