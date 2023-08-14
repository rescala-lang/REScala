package kofre.datatypes.experiments

import kofre.base.{Bottom, Lattice}
import kofre.dotted.{Dotted, HasDots}
import kofre.syntax.PermCausalMutate
import kofre.time.Dots

case class CausalDelta[A](contained: Dots, predecessors: Dots, delta: A) derives Lattice, Bottom
object CausalDelta {
  given hasDots[A: HasDots]: HasDots[CausalDelta[A]] with {
    extension (dotted: CausalDelta[A])
      def dots: Dots = dotted.contained
      def removeDots(dots: Dots): Option[CausalDelta[A]] =
        dotted.delta.removeDots(dots).map: delta =>
          val overlap = dotted.contained union dots
          CausalDelta(dotted.contained subtract dots, dotted.predecessors subtract overlap, delta)
  }
}

case class CausalStore[A](pending: CausalDelta[A], state: A)

object CausalStore {
  given lattice[A: Bottom: Lattice]: Lattice[CausalStore[A]] with {
    def merge(left: CausalStore[A], right: CausalStore[A]): CausalStore[A] =
      val pending: CausalDelta[A] = left.pending merge right.pending
      val state                   = left.state merge right.state
      if pending.predecessors contains pending.contained
      then CausalStore(Bottom.empty, pending.delta merge state)
      else CausalStore(pending, state)
  }

  given bottom[A: Bottom]: Bottom[CausalStore[A]] = Bottom.derived

  given hasDots[A: HasDots: Bottom]: HasDots[CausalStore[A]] = HasDots.derived

  given syntaxPermissions[C, L: Bottom](using outer: PermCausalMutate[C, CausalStore[L]]): PermCausalMutate[C, L]
  with {
    override def mutateContext(container: C, withContext: Dotted[L]): C =
      val res = CausalStore(CausalDelta(withContext.context, context(container), withContext.data), Bottom.empty)
      outer.mutateContext(container, withContext.map(_ => res))
    override def context(c: C): Dots = outer.context(c)
    override def query(c: C): L      = outer.query(c).state
  }
}
