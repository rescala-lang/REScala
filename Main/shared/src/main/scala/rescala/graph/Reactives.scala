package rescala.graph

import rescala.engine.Turn
import rescala.util.Globals

import scala.language.higherKinds


trait Struct { type State[P, S <: Struct] }

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
  protected[rescala] def state: S#State[Value, S]

  protected[rescala] def reevaluate(turn: Turn[S]): ReevaluationResult[Value, S]

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
  override type Value <: P
}


/** helper class implementing the state methods of reactive and pulsing */
abstract class Base[P, S <: Struct](struct: S#State[Pulse[P], S]) extends Pulsing[Pulse[P], S] {
  override type Value = Pulse[P]
  final override protected[rescala] def state: S#State[Value, S] = struct
}





