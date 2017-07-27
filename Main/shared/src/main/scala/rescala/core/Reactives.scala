package rescala.core

import scala.language.higherKinds


trait Struct { type State[P, S <: Struct] }

/** A reactive value is something that can be reevaluated
  *
  * @tparam S Defines the structure of the internal state, as used by the propagation engine.
  */
trait Reactive[S <: Struct] {

  type Value

  /** Internal state of this reactive, managed by the propagation engine */
  protected[rescala] def state: S#State[Value, S]

  protected[rescala] def reevaluate(turn: Turn[S], before: Value, indeps: Set[Reactive[S]]): ReevaluationResult[S]

}


/**
  * A reactive that user computations can read values from (e.g., before, after, etc.)
  *
  * @tparam P Value type stored by the pulse of the reactive value
  * @tparam S Struct type that defines the spore type used to manage the reactive evaluation
  */
trait ReadableReactive[+P, S <: Struct] extends Reactive[S] {
  override type Value <: P
}

/**
  * A reactive that the scheduler can write values on (i.e., reevOut)
  * @tparam P
  * @tparam S Struct type that defines the spore type used to manage the reactive evaluation
  */
trait WriteableReactive[-P, S <: Struct] extends Reactive[S] {
  override type Value >: P
}

/**
  * A base implementation for all reactives, tying together the Readable interface for user computations and the Writeable interface for schedulers.
  */
abstract class Base[P, S <: Struct](initialState: S#State[Pulse[P], S], rename: REName) extends ReadableReactive[Pulse[P], S] with WriteableReactive[Pulse[P], S] {
  override type Value = Pulse[P]
  override def toString: String = rename.name
  final override protected[rescala] def state: S#State[Value, S] = initialState
}
