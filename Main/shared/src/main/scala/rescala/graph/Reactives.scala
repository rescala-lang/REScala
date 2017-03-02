package rescala.graph

import rescala.propagation.Turn

/**
  * A reactive value is something that can be reevaluated
  *
  * @tparam S Struct type that defines the spore type used to manage the reactive evaluation
  */
trait Reactive[S <: Struct] {

  type Value
  final override val hashCode: Int = Globals.nextID().hashCode()

  /**
    * Spore that is used to internally manage the reactive evaluation of this value
    *
    * @return Spore for this value
    */
  protected[rescala] def state: S#StructType[Value, Reactive[S]]

  /**
    * Reevaluates this value when it is internally scheduled for reevaluation
    *
    * @param turn Turn that handles the reevaluation
    * @return Result of the reevaluation
    */
  protected[rescala] def reevaluate()(implicit turn: Turn[S]): ReevaluationResult[Value, S]

  /** for debugging */
  private val name = Globals.declarationLocationName()
  override def toString: String = name
}

/**
  * A pulsing value is a reactive value that stores a pulse with it's old and new value
  *
  * @tparam P Value type stored by the pulse of the reactive value
  * @tparam S Struct type that defines the spore type used to manage the reactive evaluation
  */
trait Pulsing[+P, S <: Struct] extends Reactive[S] {
  protected[rescala] def stable(implicit turn: Turn[S]): Pulse[P]
  protected[rescala] def pulse(implicit turn: Turn[S]): Pulse[P]
}


/** helper class to initialise engine and select lock */
abstract class Base[P, S <: Struct](struct: S#StructType[P, Reactive[S]]) extends Pulsing[P, S] {
  override type Value = P
  final override protected[rescala] def state: S#StructType[Value, Reactive[S]] = struct

  final protected[rescala] override def stable(implicit turn: Turn[S]): Pulse[P] = struct.base
  final protected[rescala] override def pulse(implicit turn: Turn[S]): Pulse[P] = struct.get
}
