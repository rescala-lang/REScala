package kofre.dotted

import kofre.datatypes.EnableWinsFlag
import kofre.time.{Dot, Dots}

case class DotSet(dots: Dots) {
  export dots.*
}

object DotSet {

  val empty: DotSet = DotSet(Dots.empty)

  def from(it: IterableOnce[Dot]): DotSet = DotSet(Dots.from(it))

  given hasDots: HasDots[DotSet] with {
    override def getDots(a: DotSet): Dots = a.dots
  }

  given dottedLattice: DottedLattice[DotSet] =
    new DottedLattice[DotSet] {

      override def mergePartial(left: Dotted[DotSet], right: Dotted[DotSet]): DotSet = {
        // the first `right.context` semantically should be `right.deletions`,
        // but the non-deleted values are added in the next line anyway
        val fromLeft  = left.store.dots subtract right.context
        val fromRight = right.store.dots subtract left.deletions

        DotSet(fromLeft union fromRight)
      }

      override def lteq(left: Dotted[DotSet], right: Dotted[DotSet]): Boolean = {
        (right.context contains left.context) &&
        (right.store.dots disjunct left.deletions)
      }

      override def decompose(state: Dotted[DotSet]): Iterable[Dotted[DotSet]] = {
        val added   = state.store.dots.decomposed.map(d => Dotted(DotSet(d), d))
        val removed = state.deletions.decomposed.map(d => Dotted(DotSet.empty, d))
        removed ++ added
      }
    }

}
