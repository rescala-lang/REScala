package rescala.macros

import rescala.core.ReadAs

import scala.annotation.compileTimeOnly

trait ReadableMacro[+A] extends ReadAs[A] with MacroAccess[A, ReadAs[A]]

trait MacroAccess[+A, +T] {
  /** Makes the enclosing reactive expression depend on the current value of the reactive.
    * Is an alias for [[rescala.macros.MacroAccess.apply]].
    *
    * @group accessor
    * @see apply
    */
  @compileTimeOnly("value can only be used inside of reactive expressions")
  final def value: A = throw new IllegalAccessException(s"$this.value called outside of macro")

  def resource: T

}
