package kofre.dotted

import kofre.datatypes.EnableWinsFlag
import kofre.time.{Dot, Dots}

/** DotsSets track causality of events without values.
  * They are the prototype of an [[EnableWinsFlag]]
  */
case class DotSet(repr: Dots) {
  export repr.*
}

object DotSet {

  val empty: DotSet = DotSet(Dots.empty)

  def from(it: Iterable[Dot]): DotSet = DotSet(Dots.from(it))

  given hasDots: HasDots[DotSet] with {
    override def getDots(a: DotSet): Dots = a.repr
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

      override def filter(value: DotSet, dots: Dots): Option[DotSet] =
        val res = value.repr.diff(dots)
        if res.isEmpty then None
        else Some(DotSet(res))

      override def lteq(left: Dotted[DotSet], right: Dotted[DotSet]): Boolean = {
        (left.context <= right.context) &&
        (right.store.dots disjunct left.deletions)
      }

      override def decompose(state: Dotted[DotSet]): Iterable[Dotted[DotSet]] = {
        val added = state.store.dots.decomposed.map(d => Dotted(DotSet(d), d))
        added ++ DottedLattice.decomposedDeletions(state)
      }
    }

}
