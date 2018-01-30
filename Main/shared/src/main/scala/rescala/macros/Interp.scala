package rescala.macros

import rescala.core.{ReSource, Struct}

import scala.annotation.compileTimeOnly

/** Common macro accessors for [[rescala.reactives.Signal]] and [[rescala.reactives.Event]]
  * @tparam T return type of the accessor
  * @groupname accessor Accessor and observers */
trait Interp[S <: Struct, +T] extends ReSource[S] {
  /** Makes the enclosing reactive expression depend on the current value of the reactive.
    * Is an alias for [[value]].
    * @group accessor
    * @see value*/
  @compileTimeOnly(s"$this apply can only be used inside of reactive expressions")
  final def apply(): T = throw new IllegalAccessException(s"$this.apply called outside of macro")

  /** Makes the enclosing reactive expression depend on the current value of the reactive.
    * Is an alias for [[apply]].
    * @group accessor
    * @see apply*/
  @compileTimeOnly("value can only be used inside of reactive expressions")
  final def value: T = throw new IllegalAccessException(s"$this.value called outside of macro")

  /** Interprets the internal type to the external type
    * @group internal */
  def interpret(n: Notification): T

}
