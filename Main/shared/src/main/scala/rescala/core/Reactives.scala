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
  protected[rescala] def reevaluate(turn: Turn[S], before: Value, indeps: Set[Reactive[S]]): ReevaluationResult[Value, S]
}


/**
  * A reactive with a known value type, such that user computations can read values from (e.g., before, after, etc.)
  *
  * @tparam P Value type stored by the reactive
  * @tparam S Struct type that defines the spore type used to manage the reactive evaluation
  */
trait ReactiV[+P, S <: Struct] extends Reactive[S] {
  override type Value <: P
}


/**
  * A base implementation for all reactives, tying together the Readable interface for user computations and the Writeable interface for schedulers.
  */
abstract class Base[P, S <: Struct](initialState: S#State[Pulse[P], S], rename: REName) extends RENamed(rename) with ReactiV[Pulse[P], S] with Reactive[S] {
  override type Value = Pulse[P]
  final override protected[rescala] def state: S#State[Pulse[P], S] = initialState
}
