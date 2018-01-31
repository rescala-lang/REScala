package rescala.core

import scala.language.higherKinds


/** Every [[ReSource]] has an internal data[[Struct]]ure which is externally defined by the scheduler.
  * Its main use is to allow the external algorithm to manage concurrency for the internal data.
  * Using the indirection with the State type here allows us to not have unbound type parameters everywhere. */
trait Struct { type State[V, S <: Struct, N] }

/** Source of (reactive) values, the [[Struct]] defines how the state is stored internally,
  * and how dependencies are managed.
  * State can only be accessed with a correct [[InnerTicket]].
  *
  * @tparam S [[Struct]] defining the internal state */
trait ReSource[S <: Struct] {
  type Value
  type Notification
  protected[rescala] def state: S#State[Value, S, Notification]
}

/** Makes the Value type well known, such that user computations can read values from (e.g., before, after, etc.)*/
trait ReNote[S <: Struct, +N] extends ReSource[S] {type Notification <: N}

/** A reactive value is something that can be reevaluated */
trait Reactive[S <: Struct] extends ReSource[S] {

  type ReIn = ReevTicket[Value, Notification, S]
  type Rout = Result[Value, Notification, S]

  /** called if any of the dependencies ([[ReSource]]s) changed in the current update turn,
    * after all (known) dependencies are updated */
  protected[rescala] def reevaluate(input: ReIn): Rout
}

/** Base implementation for reactives, combining [[ReNote]] for value access, with [[Reactive]] for scheduling,
  * together with a [[REName]] and asking for a [[Struct.State]]
  * @param initialState the initial state passed by the scheduler
  * @param rename the name of the reactive, useful for debugging as it often contains positional information */
abstract class Base[P, S <: Struct, N](initialState: S#State[P, S, N], rename: REName)
  extends RENamed(rename) with ReNote[S, N] with Reactive[S] {
  override type Value = P
  override type Notification = N
  final override protected[rescala] def state: S#State[P, S, N] = initialState
}
