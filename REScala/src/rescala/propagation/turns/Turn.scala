package rescala.propagation.turns

import rescala.propagation.Reactive

/**
 * The engine that schedules the (glitch-free) evaluation
 * of the nodes in the dependency graph.
 */
trait Turn {

  /** admits a new source change */
  def admit(source: Reactive)(setPulse: => Boolean): Unit

  /** called when a new reactive is created and registered into the network
    * subclasses are expected to register the reactive with its dependencies
    * and calculate the correct level*/
  def create[T <: Reactive](dependencies: Set[Reactive])(f: => T): T

  /** called when a new dynamic reactive is created and registered into the network
    * subclasess are expected to immediately evaluate the reactive
    * to enter a valid state
    * the dependencies should be used to calculate a approximation for the level */
  def createDynamic[T <: Reactive](dependencies: Set[Reactive])(f: => T): T

  /** removes reactive from its dependencies */
  def unregister(dependant: Reactive)(dependency: Reactive): Unit

  /** mark the reactive as needing a reevaluation */
  def enqueue(dep: Reactive): Unit

  /** mark the state of the reactive as changed, i.e. it needs a commit or rollback */
  def markForCommit(reactive: Reactive): Unit

  /** register an after commit handler */
  def afterCommit(handler: => Unit): Unit

  /** runs the given code while collecting dynamically used reactives */
  def collectDependencies[T](f: => T): (T, Set[Reactive])

  /** mark a reactive as dynamically used */
  def useDependency(dependency: Reactive): Unit

  /** check if the current turn hold the lock */
  def checkLock(lock: TurnLock): Boolean = true

}
