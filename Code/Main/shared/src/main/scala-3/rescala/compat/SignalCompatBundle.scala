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
    final def map[B](expression: T => B)(implicit ticket: CreationTicket): Signal[B] =
      Signals.static(this)(st => expression(st.collectStatic(this).get))

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
    inline def apply[T](inline expr: T)(using ct: CreationTicket): Signal[T] = ${rescala.macros.signalMacro[T, selfType.type , StaticTicket, Signal]('expr, 'selfType, 'ct)}
  }

}
