package kofre.dotted

import kofre.datatypes.EnableWinsFlag
import kofre.time.{Dot, Dots}

case class DotSet(dots: Dots) {
  export dots.*
}

object DotSet {

  def empty: DotSet = DotSet(Dots.empty)

  def from(it: IterableOnce[Dot]): DotSet = DotSet(Dots.from(it))

  given hasDots: HasDots[DotSet] with {
    override def dots(a: DotSet): Dots = a.dots
  }

  given contextDecompose: DottedLattice[DotSet] =
    new DottedLattice[DotSet] {

      override def mergePartial(left: Dotted[DotSet], right: Dotted[DotSet]): DotSet = {
        val fromLeft  = left.store.dots subtract right.context
        val fromRight = right.store.dots.subtract(left.context subtract left.store.dots)

        DotSet(fromLeft union fromRight)
      }

      override def lteq(left: Dotted[DotSet], right: Dotted[DotSet]): Boolean = {
        val firstCondition = left.context.forall(right.context.contains)

        val secondCondition = {
          val diff = left.context.diff(left.store.dots)
          right.store.dots.intersect(diff).isEmpty
        }

        firstCondition && secondCondition
      }

      override def decompose(state: Dotted[DotSet]): Iterable[Dotted[DotSet]] = {
        val added =
          for (d <- state.store.dots.iterator) yield
            val single = DotSet(Dots.single(d))
            Dotted(single, single.dots)
        val removed = state.context.subtract(state.store.dots).decomposed.map(Dotted(DotSet.empty, _))
        removed ++ added
      }
    }

}
