package rescala.macros

import rescala.core.{ReSourciV, Struct}

import scala.annotation.compileTimeOnly
import scala.annotation.unchecked.uncheckedVariance

/** Common macro accessors for [[rescala.reactives.Signal]] and [[rescala.reactives.Event]]
  * @tparam T return type of the accessor
  * @tparam V internal type of the reactive
  * @groupname accessor Accessor and observers */
trait MacroAccessors[+V, +T, S <: Struct] extends ReSourciV[V, S] {
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
  def interpret(v: V @uncheckedVariance): T
}
