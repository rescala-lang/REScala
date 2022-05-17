package rescala.compat

import rescala.core.Core
import rescala.interface.RescalaInterface
import rescala.macros.ReadableMacroBundle
import rescala.operator.{Operators, SignalBundle, cutOutOfUserComputation}

trait SignalCompatBundle extends ReadableMacroBundle {
  selfType: Operators =>

  trait SignalCompat[+T] extends ReadableMacro[T] {
    selfType: Signal[T] =>

    /** Return a Signal with f applied to the value
      * @group operator
      */
    @cutOutOfUserComputation
    final inline def map[B](inline expression: T => B)(using ct: CreationTicket): Signal[B] =
      Signal.dynamic(expression(this.value))

  }

  class UserDefinedFunction[+T, Dep, Cap](
      val staticDependencies: Set[Dep],
      val expression: Cap => T,
      val isStatic: Boolean = true
  )

  /** A signal expression can be used to create signals accessing arbitrary other signals.
    * Use the apply method on a signal to access its value inside of a signal expression.
    * {{{
    * val a: Signal[Int]
    * val b: Signal[Int]
    * val result: Signal[String] = Signal { a().toString + b().toString}
    * }}}
    * @group create
    */
  object Signal {
    inline def apply[T](inline expr: T)(using ct: CreationTicket): Signal[T] = ${
      rescala.macros.reactiveMacro[T, [x] =>> x, selfType.type, Signal]('expr, 'selfType, 'ct, '{ "Signal" }, 'true)
    }

    inline def dynamic[T](inline expr: T)(using ct: CreationTicket): Signal[T] = ${
      rescala.macros.reactiveMacro[T, [x] =>> x, selfType.type, Signal]('expr, 'selfType, 'ct, '{ "Signal" }, 'false)
    }
  }

}
