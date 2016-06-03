package rescala.propagation

import rescala.graph.{Pulse, Buffer, Reactive, Struct}

/**
  * The Turn interface glues the reactive interface and the propagation implementation together.
  */
trait Turn[S <: Struct] {
  /** reads the pulses of a given spore */
  def pulses[P](budP: S#SporeP[P, _]): Buffer[Pulse[P]]

  /** updates the incoming dependencies for a given spore. used for dynamic reactives */
  def updateIncoming[R](bud: S#Spore[R], newDependencies: Set[R]): Unit

  /** reads the set of incoming dependencies from a spore */
  def incoming[R](bud: S#Spore[R]): Set[R]

  /** used to create state containers of each reactive */
  def bud[P, R](initialValue: Pulse[P] = Pulse.none, transient: Boolean = true, initialIncoming: Set[R] = Set.empty[R]): S#SporeP[P, R]

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
