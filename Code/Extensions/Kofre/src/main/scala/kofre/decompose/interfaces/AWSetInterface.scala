package kofre.decompose.interfaces

import kofre.causality.{CausalContext, Dot}
import kofre.decompose.*
import kofre.syntax.OpsSyntaxHelper
import kofre.contextual.WithContextDecompose.*
import kofre.contextual.WithContext

/** An AWSet (Add-Wins Set) is a Delta CRDT modeling a set.
  *
  * When an element is concurrently added and removed/cleared from the set then the add operation wins, i.e. the resulting set contains the element.
  */
object AWSetInterface {
  type Embedded[E] = Map[E, CausalContext]
  type AWSet[E]    = WithContext[Embedded[E]]
  object AWSet:
    def empty[E]: AWSet[E] = WithContext(Map.empty, CausalContext.empty)

  extension [C, E](container: C) def asAWSet: AWSetSyntax[C, E] = AWSetSyntax(container)

  implicit class AWSetSyntax[C, E](container: C) extends OpsSyntaxHelper[C, AWSet[E]](container) {

    def elements(using QueryP): Set[E] = current.store.keySet

    def contains(elem: E)(using QueryP): Boolean = current.store.contains(elem)

    def add(e: E)(using MutationIDP): C = {
      val WithContext(dm, cc) = current
      val nextDot             = cc.max(replicaID).fold(Dot(replicaID, 0))(_.advance)
      val v                   = dm.getOrElse(e, CausalContext.empty)

      deltaState[E].make(
        dm = Map(e -> CausalContext.single(nextDot)),
        cc = v add nextDot
      )
    }

    def addAll(elems: Iterable[E])(using MutationIDP): C = {
      val WithContext(dm, cc) = current
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
      val WithContext(dm, _) = current
      val v                  = dm.getOrElse(e, CausalContext.empty)

      deltaState[E].make(
        cc = v
      )
    }

    def removeAll(elems: Iterable[E])(using MutationP): C = {
      val WithContext(dm, _) = current
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
      val WithContext(dm, _) = current
      val removedDots        = dm.collect {
        case (k, v) if cond(k) => v
      }.foldLeft(CausalContext.empty)(_ union _)

      deltaState[E].make(
        cc = removedDots
      )
    }

    def clear()(using MutationP): C = {
      val WithContext(dm, _) = current
      deltaState[E].make(
        cc = DotMap[E, CausalContext].dots(dm)
      )
    }

  }

  private class DeltaStateFactory[E] {
    val bottom: AWSet[E] = UIJDLattice[AWSet[E]].empty

    def make(
        dm: Map[E, CausalContext] = bottom.store,
        cc: CausalContext = bottom.context
    ): AWSet[E] = WithContext(dm, cc)
  }

  private def deltaState[E]: DeltaStateFactory[E] = new DeltaStateFactory[E]

}
