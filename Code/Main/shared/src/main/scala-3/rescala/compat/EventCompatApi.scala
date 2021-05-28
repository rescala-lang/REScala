package rescala.compat

import rescala.core.Core
import rescala.interface.RescalaInterface
import rescala.operator.{cutOutOfUserComputation}

trait EventCompatApi {
  selfType: RescalaInterface with Core =>


  trait EventCompat[+T] extends Interp[Option[T]] {
    selfType :Event[T] =>

    /** Filters the event, only propagating the value when the filter is true.
      * @usecase def filter(pred: T => Boolean): rescala.default.Event[T]
      * @group operator
      */
    @cutOutOfUserComputation
    final def filter(expression: T => Boolean)(implicit ticket: CreationTicket): Event[T] =
      Events.staticNamed(s"(filter $this)", this)(st => st.collectStatic(this).filter(expression))

    /** Collects the results from a partial function
      * @usecase def collect[U](pf: PartialFunction[T, U]): rescala.default.Event[U]
      * @group operator
      */
    final def collect[U](expression: PartialFunction[T, U])(implicit ticket: CreationTicket): Event[U] =
      Events.staticNamed(s"(collect $this)", this) { st => st.collectStatic(this).collect(expression) }


    /** Transform the event.
      * @usecase def map[A](expression: T => A): rescala.default.Event[A]
      * @group operator
      */
    @cutOutOfUserComputation
    final def map[A](expression: T => A)(implicit ticket: CreationTicket): Event[A] =
      Events.staticNamed(s"(map $this)", this)(st => st.collectStatic(this).map(expression))

    /** Folds events with a given operation to create a Signal.
      * @group conversion
      * @usecase def fold[A](init: A)(op: (A, T) => A): rescala.default.Signal[A]
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
    def apply[T](expr: DynamicTicket ?=> Option[T]): Event[T]   = Events.dynamic()(expr(using _))
    def dynamic[T](expr: DynamicTicket ?=> Option[T]): Event[T] = Events.dynamic()(expr(using _))
  }

}
