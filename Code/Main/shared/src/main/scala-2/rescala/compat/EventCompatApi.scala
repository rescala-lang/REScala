package rescala.compat

import rescala.core.Core
import rescala.interface.RescalaInterface
import rescala.macros.InterpBundle
import rescala.macros.MacroTags.{Dynamic, Static}
import rescala.operator.{EventsMacroImpl, cutOutOfUserComputation}

trait EventCompatApi extends InterpBundle {
  selfType: RescalaInterface with Core =>

  trait EventCompat[+T] extends InterpMacro[Option[T]] {
    selfType: Event[T] =>

    /** Collects the results from a partial function
      *
      * @usecase def collect[U](pf: PartialFunction[T, U]): rescala.default.Event[U]
      * @group operator
      */
    @cutOutOfUserComputation
    final def collect[U](expression: PartialFunction[T, U])(implicit ticket: CreationTicket): Event[U] =
      macro rescala.macros.ReactiveMacros.ReactiveUsingFunctionMacro[
        T,
        U,
        EventsMacroImpl.CollectFuncImpl.type,
        Events.type,
        StaticTicket,
        DynamicTicket,
        CreationTicket,
        LowPriorityCreationImplicits
      ]

    /** Filters the event, only propagating the value when the filter is true.
      *
      * @usecase def filter(pred: T => Boolean): rescala.default.Event[T]
      * @group operator
      */
    @cutOutOfUserComputation
    final def filter(expression: T => Boolean)(implicit ticket: CreationTicket): Event[T] =
      macro rescala.macros.ReactiveMacros.ReactiveUsingFunctionMacro[
        T,
        T,
        EventsMacroImpl.FilterFuncImpl.type,
        Events.type,
        StaticTicket,
        DynamicTicket,
        CreationTicket,
        LowPriorityCreationImplicits
      ]

    /** Filters the event, only propagating the value when the filter is true.
      *
      * @usecase def &&(pred: T => Boolean): rescala.default.Event[T]
      * @see filter
      * @group operator
      */
    @cutOutOfUserComputation
    final def &&(expression: T => Boolean)(implicit ticket: CreationTicket): Event[T] =
      macro rescala.macros.ReactiveMacros.ReactiveUsingFunctionMacro[
        T,
        T,
        EventsMacroImpl.FilterFuncImpl.type,
        Events.type,
        StaticTicket,
        DynamicTicket,
        CreationTicket,
        LowPriorityCreationImplicits
      ]

    /** Transform the event.
      *
      * @usecase def map[A](expression: T => A): rescala.default.Event[A]
      * @group operator
      */
    @cutOutOfUserComputation
    final def map[A](expression: T => A)(implicit ticket: CreationTicket): Event[A] =
      macro rescala.macros.ReactiveMacros.ReactiveUsingFunctionMacro[
        T,
        A,
        EventsMacroImpl.MapFuncImpl.type,
        Events.type,
        StaticTicket,
        DynamicTicket,
        CreationTicket,
        LowPriorityCreationImplicits
      ]

    /** Folds events with a given operation to create a Signal.
      * @group conversion
      * @usecase def fold[A](init: A)(op: (A, T) => A): rescala.default.Signal[A]
      * @inheritdoc
      */
    @cutOutOfUserComputation
    final def fold[A](init: A)(op: (A, T) => A)(implicit ticket: CreationTicket): Signal[A] =
      macro rescala.macros.ReactiveMacros.EventFoldMacro[
        T,
        A,
        Events.type,
        CreationTicket,
        StaticTicket,
        CreationTicket,
        LowPriorityCreationImplicits
      ]

  }

  /** Similar to [[Signal]] expressions, but resulting in an event.
    * Accessed events return options depending on whether they fire or not,
    * and the complete result of the expression is an event as well.
    *
    * @see [[Signal]]
    * @group create
    */
  object Event {
    final def apply[A](expression: Option[A])(implicit ticket: CreationTicket): Event[A] =
      macro rescala.macros.ReactiveMacros.ReactiveExpression[
        A,
        Static,
        Events.type,
        StaticTicket,
        DynamicTicket,
        CreationTicket,
        LowPriorityCreationImplicits
      ]
    final def static[A](expression: Option[A])(implicit ticket: CreationTicket): Event[A] =
      macro rescala.macros.ReactiveMacros.ReactiveExpression[
        A,
        Static,
        Events.type,
        StaticTicket,
        DynamicTicket,
        CreationTicket,
        LowPriorityCreationImplicits
      ]
    final def dynamic[A](expression: Option[A])(implicit ticket: CreationTicket): Event[A] =
      macro rescala.macros.ReactiveMacros.ReactiveExpression[
        A,
        Dynamic,
        Events.type,
        StaticTicket,
        DynamicTicket,
        CreationTicket,
        LowPriorityCreationImplicits
      ]
  }

}
