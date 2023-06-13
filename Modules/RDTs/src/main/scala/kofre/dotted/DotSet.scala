package kofre.dotted

import kofre.time.{Dot, Dots}

/** DotsSets track causality of events without values.
  * They are the prototype of an [[EnableWinsFlag]]
  */
case class DotSet(repr: Dots) {
  export repr.{contains, isEmpty, toSet}
}

object DotSet {

  val empty: DotSet = DotSet(Dots.empty)

  def from(it: Iterable[Dot]): DotSet = DotSet(Dots.from(it))

  given hasDots: HasDots[DotSet] with {
    extension(value: DotSet)
      override def dots: Dots = value.repr

      override def removeDots(dots: Dots): Option[DotSet] =
        val res = value.repr.diff(dots)
        if res.isEmpty then None
        else Some(DotSet(res))
  }

  given dottedLattice: DottedLattice[DotSet] =
    new DottedLattice[DotSet] {

      override def mergePartial(left: Dotted[DotSet], right: Dotted[DotSet]): DotSet = {
        // the first `right.context` semantically should be `right.deletions`,
        // but the non-deleted values are added in the next line anyway
        val fromLeft  = left.data.dots subtract right.context
        val fromRight = right.data.dots subtract left.deletions

        DotSet(fromLeft union fromRight)
      }


      override def lteq(left: Dotted[DotSet], right: Dotted[DotSet]): Boolean = {
        (left.context <= right.context) &&
        (right.data.dots disjunct left.deletions)
      }

      override def decompose(state: Dotted[DotSet]): Iterable[Dotted[DotSet]] = {
        val added = state.data.dots.decomposed.map(d => Dotted(DotSet(d), d))
        added ++ DottedLattice.decomposedDeletions(state)
      }
    }

}
