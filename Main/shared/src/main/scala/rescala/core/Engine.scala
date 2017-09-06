package rescala.core

import rescala.RescalaDefaultImports

import scala.annotation.implicitNotFound

/**
  * Propagation engine that defines the basic data-types available to the user and creates turns for propagation handling
  *
  * @tparam S Struct type that defines the spore type used to manage the reactive evaluation
  */
@implicitNotFound(msg = "Could not find an implicit propagation engine. Did you forget an import?")
trait Engine[S <: Struct] extends RescalaDefaultImports[S] {

  override def explicitEngine: this.type = this

  private[rescala] def executeTurn[I, R](initialWrites: Traversable[Reactive], admissionPhase: AdmissionTicket => I, wrapUpPhase: (I, WrapUpTicket) => R): R
  private[rescala] def singleNow[A](reactive: ReactiV[A, S]): A
  private[rescala] def create[T](f: (Creation) => T): T
}

