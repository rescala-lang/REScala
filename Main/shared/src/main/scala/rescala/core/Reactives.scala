package rescala.core

import scala.annotation.compileTimeOnly
import scala.language.higherKinds


/** Every [[ReSource]] has an internal data[[Struct]]ure which is externally defined by the scheduler.
  * Its main use is to allow the external algorithm to manage concurrency for the internal data.
  * Using the indirection with the State type here allows us to not have unbound type parameters everywhere. */
trait Struct { type State[V, S <: Struct] }

/** Source of (reactive) values, the [[Struct]] defines how the state is stored internally,
  * and how dependencies are managed.
  * State can only be accessed with a correct [[InnerTicket]].
  *
  * @tparam S [[Struct]] defining the internal state */
trait ReSource[S <: Struct] {
  type Value
  final type State = S#State[Value, S]
  protected[rescala] def state: State
}

/** A reactive value is something that can be reevaluated */
trait Reactive[S <: Struct] extends ReSource[S] {

  final type ReIn = ReevTicket[Value, S]
  final type Rout = Result[Value, S]

  /** called if any of the dependencies ([[ReSource]]s) changed in the current update turn,
    * after all (known) dependencies are updated */
  protected[rescala] def reevaluate(input: ReIn): Rout
}

/** Base implementation for reactives, with [[Reactive]] for scheduling,
  * together with a [[REName]] and asking for a [[Struct.State]]
  * @param initialState the initial state passed by the scheduler
  * @param rename the name of the reactive, useful for debugging as it often contains positional information */
abstract class Base[V, S <: Struct](initialState: S#State[V, S], rename: REName)
  extends RENamed(rename) with Reactive[S] {
  override type Value = V
  final override protected[rescala] def state: State = initialState
}


/** Common macro accessors for [[rescala.reactives.Signal]] and [[rescala.reactives.Event]]
  * @tparam A return type of the accessor
  * @groupname accessor Accessor and observers */
trait Interp[+A, S <: Struct] extends ReSource[S] {
  /** Makes the enclosing reactive expression depend on the current value of the reactive.
    * Is an alias for [[value]].
    * @group accessor
    * @see value*/
  @compileTimeOnly(s"$this apply can only be used inside of reactive expressions")
  final def apply(): A = throw new IllegalAccessException(s"$this.apply called outside of macro")

  /** Makes the enclosing reactive expression depend on the current value of the reactive.
    * Is an alias for [[apply]].
    * @group accessor
    * @see apply*/
  @compileTimeOnly("value can only be used inside of reactive expressions")
  final def value: A = throw new IllegalAccessException(s"$this.value called outside of macro")

  /** Interprets the internal type to the external type
    * @group internal */
  def interpret(v: Value): A

}
