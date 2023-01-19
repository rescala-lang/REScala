package rescala.compat

import rescala.core.{LowPriorityScopeImplicits, ReSource, ScopeSearch}
import rescala.macros.MacroTags.{Dynamic, Static}
import rescala.macros.ReadableMacroBundle
import rescala.operator.{Operators, cutOutOfUserComputation}

trait SignalCompatBundle extends ReadableMacroBundle {
  selfType: Operators =>

  trait SignalCompat[+T] extends ReadableMacro[T] {

    /** Return a Signal with f applied to the value
      * @group operator
      */
    @cutOutOfUserComputation
    final def map[B](expression: T => B)(implicit ticket: CreationTicket): Signal[B] =
      macro rescala.macros.ReactiveMacros.ReactiveUsingFunctionMacro[
        T,
        B,
        rescala.operator.SignalMacroImpl.MapFuncImpl.type,
        Signals.type,
        StaticTicket,
        DynamicTicket,
        ScopeSearch[State],
        LowPriorityScopeImplicits,
        ReSource.of[State]
      ]

  }

  class UserDefinedFunction[+T, Dep, Cap](
      val staticDependencies: Set[Dep],
      val expression: Cap => T,
      val isStatic: Boolean = true
  )

  object UserDefinedFunction {
    implicit def fromExpression[T, Dep, Cap](expression: => T): UserDefinedFunction[T, Dep, Cap] =
      macro rescala.macros.ReactiveMacros.UDFExpressionWithAPI[
        T,
        Dep,
        Cap,
        DynamicTicket,
        ScopeSearch[State],
        LowPriorityScopeImplicits,
        ReSource.of[State]
      ]
  }

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
    final def apply[A](expression: A)(implicit ticket: CreationTicket): Signal[A] =
      macro rescala.macros.ReactiveMacros.ReactiveExpression[
        A,
        Static,
        Signals.type,
        StaticTicket,
        DynamicTicket,
        ScopeSearch[State],
        LowPriorityScopeImplicits,
        ReSource.of[State]
      ]
    final def static[A](expression: A)(implicit ticket: CreationTicket): Signal[A] =
      macro rescala.macros.ReactiveMacros.ReactiveExpression[
        A,
        Static,
        Signals.type,
        StaticTicket,
        DynamicTicket,
        ScopeSearch[State],
        LowPriorityScopeImplicits,
        ReSource.of[State]
      ]
    final def dynamic[A](expression: A)(implicit ticket: CreationTicket): Signal[A] =
      macro rescala.macros.ReactiveMacros.ReactiveExpression[
        A,
        Dynamic,
        Signals.type,
        StaticTicket,
        DynamicTicket,
        ScopeSearch[State],
        LowPriorityScopeImplicits,
        ReSource.of[State]
      ]
  }

}
