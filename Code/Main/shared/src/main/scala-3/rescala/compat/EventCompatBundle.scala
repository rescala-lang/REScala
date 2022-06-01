package rescala.compat

import rescala.core.Core
import rescala.interface.RescalaInterface
import rescala.operator.cutOutOfUserComputation
import rescala.operator.Operators
import rescala.macros.ReadableMacroBundle

trait EventCompatBundle extends ReadableMacroBundle {
  bundle: Operators =>

  trait EventCompat[+T] extends ReadableMacro[Option[T]] {
    selfType: Event[T] =>

    /** Filters the event, only propagating the value when the filter is true.
      * @group operator
      */
    @cutOutOfUserComputation
    final inline def filter(inline expression: T => Boolean)(implicit ticket: CreationTicket): Event[T] =
      Event.dynamic { this.value.filter(expression) }

    /** Filters the event, only propagating the value when the filter is true.
      * @group operator
      */
    @cutOutOfUserComputation
    final infix inline def &&(inline expression: T => Boolean)(implicit ticket: CreationTicket): Event[T] =
      Event.dynamic { this.value.filter(expression) }

    /** Collects the results from a partial function
      * @group operator
      */
    final inline def collect[U](inline expression: PartialFunction[T, U])(implicit ticket: CreationTicket): Event[U] =
      Event.dynamic { this.value.collect(expression) }

    /** Transform the event.
      * @group operator
      */
    @cutOutOfUserComputation
    final inline def map[B](inline expression: T => B)(implicit ticket: CreationTicket): Event[B] =
      Event.dynamic { this.value.map(expression) }

    /** Folds events with a given operation to create a Signal.
      * @group conversion
      * @inheritdoc
      */
    @cutOutOfUserComputation
    final def fold[A](init: A)(op: (A, T) => A)(implicit ticket: CreationTicket): Signal[A] =
      Events.foldOne(this, init)(op)

  }

  /** Similar to [[Signal]] expressions, but resulting in an event.
    * Accessed events return options depending on whether they fire or not,
    * and the complete result of the expression is an event as well.
    *
    * @see [[Signal]]
    * @group create
    */
  object Event {
    inline def apply[T](inline expr: Option[T])(using ct: CreationTicket): Event[T] = {
      val (sources, fun) = rescala.macros.getDependencies[Option, T, ReSource, StaticTicket, true](expr)
      bundle.Events.static(sources: _*)(fun)
      // ${ rescala.macros.reactiveMacro[T, Option, moduleType.type, Event]('expr, 'moduleType, 'ct, '{ "Event" }, 'true) }
    }
    inline def dynamic[T](inline expr: Option[T])(using ct: CreationTicket): Event[T] = {
      val (sources, fun) = rescala.macros.getDependencies[Option, T, ReSource, DynamicTicket, false](expr)
      bundle.Events.dynamic(sources: _*)(fun)
      // ${
      //  rescala.macros.reactiveMacro[T, Option, moduleType.type, Event]('expr, 'moduleType, 'ct, '{ "Event" }, 'false)
      // }
    }
  }

}
