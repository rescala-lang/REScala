package kofre.predef

import kofre.base.DecomposeLattice
import kofre.causality.{CausalContext, Dot}
import kofre.contextual.WithContext
import kofre.contextual.ContextDecompose.*
import kofre.decompose.*
import kofre.predef.AddWinsSet
import kofre.predef.AddWinsSet.Embedded
import kofre.syntax.OpsSyntaxHelper


case class AddWinsSet[E](inner: WithContext[Embedded[E]])

/** An AddWinsSet (Add-Wins Set) is a Delta CRDT modeling a set.
  *
  * When an element is concurrently added and removed/cleared from the set then the add operation wins, i.e. the resulting set contains the element.
  */
object AddWinsSet {
  type Embedded[E] = Map[E, CausalContext]

  def empty[E]: AddWinsSet[E] = AddWinsSet(WithContext(Map.empty, CausalContext.empty))

  given awsetLattice[E]: DecomposeLattice[AddWinsSet[E]] = DecomposeLattice.derived

  implicit class AWSetSyntax[C, E](container: C) extends OpsSyntaxHelper[C, AddWinsSet[E]](container) {

    def elements(using QueryP): Set[E] = current.inner.store.keySet

    def contains(elem: E)(using QueryP): Boolean = current.inner.store.contains(elem)

    def add(e: E)(using MutationIDP): C = {
      val WithContext(dm, cc) = current.inner
      val nextDot             = cc.max(replicaID).fold(Dot(replicaID, 0))(_.advance)
      val v                   = dm.getOrElse(e, CausalContext.empty)

      deltaState[E].make(
        dm = Map(e -> CausalContext.single(nextDot)),
        cc = v add nextDot
      )
    }

    def addAll(elems: Iterable[E])(using MutationIDP): C = {
      val WithContext(dm, cc) = current.inner
      val nextCounter         = cc.nextTime(replicaID)
      val nextDots            = CausalContext.fromSet((nextCounter until nextCounter + elems.size).map(Dot(replicaID, _)))

      val ccontextSet = elems.foldLeft(nextDots) {
        case (dots, e) => dm.get(e) match {
            case Some(ds) => dots union ds
            case None     => dots
          }
      }

      deltaState[E].make(
        dm = (elems zip nextDots.iterator.map(CausalContext.single)).toMap,
        cc = ccontextSet
      )
    }

    def remove(e: E)(using MutationP): C = {
      val WithContext(dm, _) = current.inner
      val v                  = dm.getOrElse(e, CausalContext.empty)

      deltaState[E].make(
        cc = v
      )
    }

    def removeAll(elems: Iterable[E])(using MutationP): C = {
      val WithContext(dm, _) = current.inner
      val dotsToRemove       = elems.foldLeft(CausalContext.empty) {
        case (dots, e) => dm.get(e) match {
            case Some(ds) => dots union ds
            case None     => dots
          }
      }

      deltaState[E].make(
        cc = dotsToRemove
      )
    }

    def removeBy(cond: E => Boolean)(using MutationP): C = {
      val WithContext(dm, _) = current.inner
      val removedDots        = dm.collect {
        case (k, v) if cond(k) => v
      }.foldLeft(CausalContext.empty)(_ union _)

      deltaState[E].make(
        cc = removedDots
      )
    }

    def clear()(using MutationP): C = {
      val WithContext(dm, _) = current.inner
      deltaState[E].make(
        cc = DotMap[E, CausalContext].dots(dm)
      )
    }

  }

  private class DeltaStateFactory[E] {

    def make(
        dm: Map[E, CausalContext] = empty.inner.store,
        cc: CausalContext = empty.inner.context
    ): AddWinsSet[E] = AddWinsSet(WithContext(dm, cc))
  }

  private def deltaState[E]: DeltaStateFactory[E] = new DeltaStateFactory[E]

}
