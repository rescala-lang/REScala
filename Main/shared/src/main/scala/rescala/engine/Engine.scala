package rescala.engine

import rescala.RescalaDefaultImports
import rescala.graph.Struct

import scala.annotation.implicitNotFound

/**
  * Propagation engine that defines the basic data-types available to the user and creates turns for propagation handling
  *
  * @tparam S Struct type that defines the spore type used to manage the reactive evaluation
  * @tparam TTurn Turn type used by the engine
  */
@implicitNotFound(msg = "Could not find an implicit propagation engine. Did you forget an import?")
trait Engine[S <: Struct, +TTurn <: Turn[S]] extends RescalaDefaultImports[S] {
  override def explicitEngine: this.type = this

  private[rescala] def executeTurn[I, R](initialWrites: Traversable[Reactive], admissionPhase: TTurn => I, wrapUpPhase: (I, TTurn) => R): R
  private[rescala] def currentTurn(): Option[TTurn]
}

