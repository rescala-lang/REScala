package rescala.core

import scala.language.higherKinds


/** Every [[ReSource]] has an internal data[[Struct]]ure which is externally defined by the scheduler.
  * Its main use is to allow the external algorithm to manage concurrency for the internal data.
  * Using the indirection with the State type here allows us to not have unbound type parameters everywhere. */
trait Struct { type State[P, S <: Struct] }

/** Source of (reactive) values, the [[Struct]] defines how the state is stored internally,
  * and how dependencies are managed.
  * State can only be accessed with a correct [[Ticket]].
  *
  * @tparam S [[Struct]] defining the internal state */
trait ReSource[S <: Struct] {
  type Value
  protected[rescala] def state: S#State[Value, S]
}

/** Makes the Value type well known, such that user computations can read values from (e.g., before, after, etc.)*/
trait ReSourciV[+P, S <: Struct] extends ReSource[S] { type Value <: P}

/** A reactive value is something that can be reevaluated */
trait Reactive[S <: Struct] extends ReSource[S] {

  /** called if any of the dependencies ([[ReSource]]s) changed in the current update turn,
    * after all (known) dependencies are updated */
  protected[rescala] def reevaluate(ticket: ReevTicket[Value, S], before: Value): Result[Value, S]
}

/** Base implementation for reactives, combining [[ReSourciV]] for value access, with [[Reactive]] for scheduling,
  * together with a [[REName]] and asking for a [[Struct.State]]
  * @param initialState the initial state passed by the scheduler
  * @param rename the name of the reactive, useful for debugging as it often contains positional information */
abstract class Base[P, S <: Struct](initialState: S#State[P, S], rename: REName)
  extends RENamed(rename) with  ReSourciV[P, S] with Reactive[S] {
  override type Value = P
  final override protected[rescala] def state: S#State[P, S] = initialState
}
