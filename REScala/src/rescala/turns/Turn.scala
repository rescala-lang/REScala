package rescala.turns

import rescala.graph.{Stateful, Commitable, Reactive}

/**
 * The engine that schedules the (glitch-free) evaluation
 * of the nodes in the dependency graph.
 */
trait Turn {

  /** returns the engine of this turn */
  def engine: Engine[Turn]

  /** allow turn to handle dynamic access to reactives */
  def accessDynamic(dependency: Reactive): Unit

  /** admits a new source change */
  def admit(reactive: Reactive): Unit

  /** removes a reactive from evaluation*/
  def forget(reactive: Reactive): Unit

  /** called when a new reactive is created and registered into the network
    * subclasses are expected to register the reactive with its dependencies
    * and calculate the correct level */
  def create[T <: Reactive](dependencies: Set[Reactive])(f: => T): T

  /** called when a new dynamic reactive is created and registered into the network
    * subclasess are expected to immediately evaluate the reactive
    * to enter a valid state
    * the dependencies should be used to calculate a approximation for the level */
  def createDynamic[T <: Reactive](dependencies: Set[Reactive])(f: => T): T

  /** removes reactive from its dependencies */
  def unregister(sink: Reactive)(source: Reactive): Unit

  /** mark the state of the reactive as changed, i.e. it needs a commit or rollback */
  def plan(commitable: Commitable): Unit

  /** register an after commit handler, these run after all reactives are commited, but before the turn finishes
    * that is, a transaction should not roll back beacause of a handler, but it may block other waiting transactions */
  def afterCommit(handler: => Unit): Unit
}
