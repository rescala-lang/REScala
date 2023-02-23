package rescala.compat

import rescala.core.{LowPriorityScopeImplicits, ReSource, ScopeSearch, CreationTicket, StaticTicket, DynamicTicket}
import rescala.macros.MacroTags.{Dynamic, Static}
import rescala.operator.{EventsMacroImpl, Operators, cutOutOfUserComputation}
import rescala.macros.ReadableMacro

trait EventCompatBundle {
  selfType: Operators =>

  trait EventCompat[+T] extends ReadableMacro[Option[T]] {
    selfType: Event[T] =>

    /** Collects the results from a partial function
      *
      * @group operator
      */
    @cutOutOfUserComputation
    final def collect[U](expression: PartialFunction[T, U])(implicit ticket: CreationTicket[State]): Event[U] =
      macro rescala.macros.ReactiveMacros.ReactiveUsingFunctionMacro[
        T,
        U,
        EventsMacroImpl.CollectFuncImpl.type,
        Events.type,
        StaticTicket[State],
        DynamicTicket[State],
        ScopeSearch[State],
        LowPriorityScopeImplicits,
        ReSource,
        State[Any]
      ]

    /** Filters the event, only propagating the value when the filter is true.
      *
      * @group operator
      */
    @cutOutOfUserComputation
    final def filter(expression: T => Boolean)(implicit ticket: CreationTicket[State]): Event[T] =
      macro rescala.macros.ReactiveMacros.ReactiveUsingFunctionMacro[
        T,
        T,
        EventsMacroImpl.FilterFuncImpl.type,
        Events.type,
        StaticTicket[State],
        DynamicTicket[State],
        ScopeSearch[State],
        LowPriorityScopeImplicits,
        ReSource,
        State[Any]
      ]

    /** Filters the event, only propagating the value when the filter is true.
      *
      * @see filter
      * @group operator
      */
    @cutOutOfUserComputation
    final def &&(expression: T => Boolean)(implicit ticket: CreationTicket[State]): Event[T] =
      macro rescala.macros.ReactiveMacros.ReactiveUsingFunctionMacro[
        T,
        T,
        EventsMacroImpl.FilterFuncImpl.type,
        Events.type,
        StaticTicket[State],
        DynamicTicket[State],
        ScopeSearch[State],
        LowPriorityScopeImplicits,
        ReSource,
        State[Any]
      ]

    /** Transform the event.
      *
      * @group operator
      */
    @cutOutOfUserComputation
    final def map[A](expression: T => A)(implicit ticket: CreationTicket[State]): Event[A] =
      macro rescala.macros.ReactiveMacros.ReactiveUsingFunctionMacro[
        T,
        A,
        EventsMacroImpl.MapFuncImpl.type,
        Events.type,
        StaticTicket[State],
        DynamicTicket[State],
        ScopeSearch[State],
        LowPriorityScopeImplicits,
        ReSource,
        State[Any]
      ]

    /** Folds events with a given operation to create a Signal.
      * @group conversion
      */
    @cutOutOfUserComputation
    final def fold[A](init: A)(op: (A, T) => A)(implicit ticket: CreationTicket[State]): Signal[A] =
      macro rescala.macros.ReactiveMacros.EventFoldMacro[
        T,
        A,
        EventsMacroImpl.FoldFuncImpl.type,
        Events.type,
        CreationTicket[State],
        StaticTicket[State],
        ScopeSearch[State],
        LowPriorityScopeImplicits,
        ReSource,
        State[Any]
      ]

  }

  /** Similar to [[rescala.compat.SignalCompatBundle.Signal]] expressions, but resulting in an event.
    * Accessed events return options depending on whether they fire or not,
    * and the complete result of the expression is an event as well.
    *
    * @see [[rescala.compat.SignalCompatBundle.Signal]]
    * @group create
    */
  object Event {
    final def apply[A](expression: Option[A])(implicit ticket: CreationTicket[State]): Event[A] =
      macro rescala.macros.ReactiveMacros.ReactiveExpression[
        A,
        Static,
        Events.type,
        StaticTicket[State],
        DynamicTicket[State],
        ScopeSearch[State],
        LowPriorityScopeImplicits,
        ReSource,
        State[Any]
      ]
    final def static[A](expression: Option[A])(implicit ticket: CreationTicket[State]): Event[A] =
      macro rescala.macros.ReactiveMacros.ReactiveExpression[
        A,
        Static,
        Events.type,
        StaticTicket[State],
        DynamicTicket[State],
        ScopeSearch[State],
        LowPriorityScopeImplicits,
        ReSource,
        State[Any]
      ]
    final def dynamic[A](expression: Option[A])(implicit ticket: CreationTicket[State]): Event[A] =
      macro rescala.macros.ReactiveMacros.ReactiveExpression[
        A,
        Dynamic,
        Events.type,
        StaticTicket[State],
        DynamicTicket[State],
        ScopeSearch[State],
        LowPriorityScopeImplicits,
        ReSource,
        State[Any]
      ]
  }

}
