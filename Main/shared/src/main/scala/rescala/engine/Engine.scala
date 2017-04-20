package rescala.engine

import rescala.RescalaDefaultImports
import rescala.graph.Struct

import scala.annotation.implicitNotFound
import scala.language.experimental.macros

/**
  * Propagation engine that defines the basic data-types available to the user and creates turns for propagation handling
  *
  * @tparam S Struct type that defines the spore type used to manage the reactive evaluation
  * @tparam TTurn Turn type used by the engine
  */
@implicitNotFound(msg = "Could not find an implicit propagation engine. Did you forget an import?")
trait Engine[S <: Struct, +TTurn <: Turn[S]] extends RescalaDefaultImports[S] {

  override def explicitEngine: this.type = this

  /**
    * Creates and executes a full turn by running through all of its phases.
    *
    * @param initialWrites Initially modified reactive values that are the source of the turn's propagation
    * @param admissionPhase Function executed between the preparation and the propagation phase
    * @tparam R Result type of the admission function
    * @return Result of the admission function
    */
  def transaction[R](initialWrites: Reactive*)(admissionPhase: TTurn => R): R

  /**
    * If there is already a current turn running, the given function is applied onto it. Otherwise, a new full turn
    * is executed with all of its phases.
    *
    * @param initialWrites Initially modified reactive values that are the source of a new turn's propagation
    * @param admissionPhase Function executed on the existing turn or between the preparation and the propagation phase of a new turn.
    * @tparam T Result type of the admission function
    * @return Result of the admission function
    */
  def subplan[T](initialWrites: Reactive*)(admissionPhase: TTurn => T): T
}
