package kofre.dotted

import kofre.time.{Dot, Dots}
import kofre.contextual.ContextDecompose.FromConlattice
import kofre.contextual.{AsCausalContext, ContextDecompose, ContextLattice, WithContext}
import kofre.decompose.interfaces


case class DotSet(repr: Dots) {
  def dots: Dots = repr
  export repr.*
}

object DotSet {

  def empty: DotSet = DotSet(Dots.empty)

  given asCausalContext: AsCausalContext[DotSet] with {
    override def dots(a: DotSet): Dots = a.repr
  }

  /** This essentially tracks the currently present dots, and all dots */
  given contextLattice: ContextLattice[DotSet] with {
    override def mergePartial(left: WithContext[DotSet], right: WithContext[DotSet]): DotSet = {
      val fromLeft  = left.store.repr subtract right.context
      val fromRight = right.store.repr.subtract(left.context subtract left.store.repr)

      DotSet(fromLeft union fromRight)
    }
  }

  /** DotSet is a dot store implementation that is simply a set of dots. See [[interfaces.EnableWinsFlag]] for a
    * usage example.
    */
  given contextDecompose: ContextDecompose[DotSet] =
    new FromConlattice[DotSet](contextLattice) {

      override def empty: WithContext[DotSet] = WithContext(DotSet.empty)

      override def lteq(left: WithContext[DotSet], right: WithContext[DotSet]): Boolean = {
        val firstCondition = left.context.forall(right.context.contains)

        val secondCondition = {
          val diff = left.context.diff(left.store.repr)
          right.store.repr.intersect(diff).isEmpty
        }

        firstCondition && secondCondition
      }

      override def decompose(state: WithContext[DotSet]): Iterable[WithContext[DotSet]] = {
        val added =
          for (d <- state.store.repr.iterator) yield
            val single = DotSet(Dots.single(d))
            WithContext(single, single.repr)
        val removed = state.context.subtract(state.store.repr).decomposed.map(WithContext(DotSet.empty, _))
        removed ++ added
      }
    }

}
