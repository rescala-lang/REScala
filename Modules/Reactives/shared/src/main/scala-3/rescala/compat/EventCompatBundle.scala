package rescala.compat

import rescala.core.{CreationTicket, DynamicTicket, ReSource, StaticTicket}
import rescala.interface.RescalaInterface
import rescala.operator.{Operators, Pulse, cutOutOfUserComputation}
import rescala.macros.{ReadableMacro}

trait EventCompatBundle {
  bundle: Operators =>

  trait EventCompat[+T] extends ReadableMacro[Option[T]] {
    selfType: Event[T] =>

    /** Filters the event, only propagating the value when the filter is true.
      * @group operator
      */
    @cutOutOfUserComputation
    final inline def filter(inline expression: T => Boolean)(implicit ticket: CreationTicket[State]): Event[T] =
      Event.dynamic { this.value.filter(expression) }

    /** Filters the event, only propagating the value when the filter is true.
      * @group operator
      */
    @cutOutOfUserComputation
    final infix inline def &&(inline expression: T => Boolean)(implicit ticket: CreationTicket[State]): Event[T] =
      Event.dynamic { this.value.filter(expression) }

    /** Collects the results from a partial function
      * @group operator
      */
    final inline def collect[U](inline expression: PartialFunction[T, U])(implicit
        ticket: CreationTicket[State]
    ): Event[U] =
      Event.dynamic { this.value.collect(expression) }

    /** Transform the event.
      * @group operator
      */
    @cutOutOfUserComputation
    final inline def map[B](inline expression: T => B)(implicit ticket: CreationTicket[State]): Event[B] =
      Event.dynamic { this.value.map(expression) }

    /** Folds events with a given operation to create a Signal.
      * @group conversion
      * @inheritdoc
      */
    @cutOutOfUserComputation
    final def fold[A](init: A)(op: (A, T) => A)(implicit ticket: CreationTicket[State]): Signal[A] =
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
    inline def apply[T](inline expr: Option[T])(using ct: CreationTicket[State]): Event[T] = {
      val (sources, fun, isStatic) =
        rescala.macros.getDependencies[Option[T], ReSource.of[State], StaticTicket[State], true](expr)
      bundle.Events.static(sources: _*)(fun)
    }
    inline def dynamic[T](inline expr: Option[T])(using ct: CreationTicket[State]): Event[T] = {
      val (sources, fun, isStatic) =
        rescala.macros.getDependencies[Option[T], ReSource.of[State], DynamicTicket[State], false](expr)
      bundle.Events.dynamic(sources: _*)(fun)
    }
  }

  object Fold {
    inline def branch[T](inline expr: FoldState[T] ?=> T): Branch[T] = {
      val (sources, fun, isStatic) =
        rescala.macros.getDependencies[FoldState[T] ?=> T, ReSource.of[State], DynamicTicket[State], false](expr)
      Branch(sources, isStatic, fun)
    }

    class Branch[S](
        val staticDependencies: List[ReSource.of[State]],
        val isStatic: Boolean,
        val run: DynamicTicket[State] => FoldState[S] ?=> S
    )

    def apply[T](init: T)(branches: Branch[T]*)(using ticket: CreationTicket[State]): Signal[T] = {

      val staticDeps = branches.iterator.flatMap(_.staticDependencies).toSet
      val isStatic   = branches.forall(_.isStatic)

      def operator(dt: DynamicTicket[State], state: () => T): T =
        branches.foldLeft(state()) { (curr, b) => b.run(dt)(using FoldState(curr)) }

      ticket.create(
        staticDeps,
        Pulse.tryCatch[T](Pulse.Value(init)),
        needsReevaluation = !isStatic
      ) { state => new SignalImpl[T](state, operator, ticket.rename, if isStatic then None else Some(staticDeps)) }
    }
  }

  inline def current[S](using fs: FoldState[S]): S = FoldState.unwrap(fs)

  extension [T](e: Event[T]) {
    inline def act[S](inline f: FoldState[S] ?=> T => S): Fold.Branch[S] = Fold.branch { e.value.fold(current)(f) }
  }

}

opaque type FoldState[T] = T
object FoldState {
  def unwrap[T](fs: FoldState[T]): T = fs
  def apply[T](t: T): FoldState[T]   = t
}
