package rescala.operator

import rescala.core.{CreationTicket, DynamicTicket, ReSource}
import rescala.structure.{Pulse, SignalImpl}

trait FoldBundle {
  bundle: Operators =>

  /** Folds when any one of a list of events occurs, if multiple events occur, every fold is executed in order.
    *
    * Example for a counter that can be reset:
    * {{{
    *   Fold(0)(
    *     add act { x => current + v },
    *     reset act { _ => 0 }
    *   )
    * }}}
    */
  object Fold {

    /** Fold branches allow to define more complex fold logic */
    inline def branch[T](inline expr: FoldState[T] ?=> T): Branch[T] = {
      val (sources, fun, isStatic) =
        rescala.macros.getDependencies[FoldState[T] ?=> T, ReSource.of[BundleState], DynamicTicket[BundleState], false](
          expr
        )
      Branch(sources, isStatic, fun)
    }

    class Branch[S](
        val staticDependencies: List[ReSource.of[BundleState]],
        val isStatic: Boolean,
        val run: DynamicTicket[BundleState] => FoldState[S] ?=> S
    )

    def apply[T](init: => T)(branches: Branch[T]*)(using ticket: CreationTicket[BundleState]): Signal[T] = {

      val staticDeps = branches.iterator.flatMap(_.staticDependencies).toSet
      val isStatic   = branches.forall(_.isStatic)

      def operator(dt: DynamicTicket[BundleState], state: () => T): T =
        var extracted = Option.empty[T]
        def curr: T   = extracted.getOrElse(state())
        branches.foreach { b =>
          extracted = Some(b.run(dt)(using FoldState(curr)))
        }
        curr

      ticket.create(
        staticDeps,
        Pulse.tryCatch[T](Pulse.Value(init)),
        needsReevaluation = !isStatic
      ) { state =>
        new SignalImpl(state, operator, ticket.info, if isStatic then None else Some(staticDeps)) with Signal[T]
      }
    }
  }

  inline def current[S](using fs: FoldState[S]): S = FoldState.unwrap(fs)

  extension [T](e: Event[T]) {
    inline def act[S](inline f: FoldState[S] ?=> T => S): Fold.Branch[S] = Fold.branch { e.value.fold(current)(f) }
  }

}

opaque type FoldState[T] = () => T
object FoldState {
  inline def unwrap[T](inline fs: FoldState[T]): T  = fs()
  inline def apply[T](inline t: => T): FoldState[T] = () => t
}
