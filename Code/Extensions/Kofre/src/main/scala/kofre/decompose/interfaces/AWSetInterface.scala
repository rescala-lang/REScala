package kofre.decompose.interfaces

import kofre.causality.{CausalContext, Dot}
import kofre.decompose.*
import kofre.syntax.{DeltaMutator, OpsSyntaxHelper}
import kofre.decompose.DotStore.*
import kofre.dotbased.CausalStore

/** An AWSet (Add-Wins Set) is a Delta CRDT modeling a set.
  *
  * When an element is concurrently added and removed/cleared from the set then the add operation wins, i.e. the resulting set contains the element.
  */
object AWSetInterface {
  type Embedded[E] = DotMap[E, DotSet]
  type AWSet[E]    = CausalStore[Embedded[E]]

  extension [C, E](container: C) def asAWSet: AWSetSyntax[C, E] = AWSetSyntax(container)

  implicit class AWSetSyntax[C, E](container: C) extends OpsSyntaxHelper[C, AWSet[E]](container) {

    def elements(using QueryP): Set[E] = current.store.keySet

    def add(e: E)(using MutationIDP): C = {
      val CausalStore(dm, cc) = current
      val nextDot             = cc.max(replicaID).fold(Dot(replicaID, 0))(_.advance)
      val v                   = dm.getOrElse(e, DotSet.empty)

      deltaState[E].make(
        dm = DotMap[E, DotSet].empty.updated(e, Set(nextDot)),
        cc = CausalContext.fromSet(v + nextDot)
      )
    }

    def addAll(elems: Iterable[E])(using MutationIDP): C = {
      val CausalStore(dm, cc) = current
      val nextCounter         = cc.nextTime(replicaID)
      val nextDots            = (nextCounter until nextCounter + elems.size).toSet.map(Dot(replicaID, _))

      val ccontextSet = elems.foldLeft(nextDots) {
        case (dots, e) => dm.get(e) match {
            case Some(ds) => dots union ds
            case None     => dots
          }
      }

      deltaState[E].make(
        dm = (elems zip nextDots.map(Set(_))).toMap,
        cc = CausalContext.fromSet(ccontextSet)
      )
    }

    def remove(e: E)(using MutationP): C = {
      val CausalStore(dm, _) = current
      val v                  = dm.getOrElse(e, DotSet.empty)

      deltaState[E].make(
        cc = CausalContext.fromSet(v)
      )
    }

    def removeAll(elems: Iterable[E])(using MutationP): C = {
      val CausalStore(dm, _) = current
      val dotsToRemove = elems.foldLeft(Set.empty[Dot]) {
        case (dots, e) => dm.get(e) match {
            case Some(ds) => dots union ds
            case None     => dots
          }
      }

      deltaState[E].make(
        cc = CausalContext.fromSet(dotsToRemove)
      )
    }

    def removeBy(cond: E => Boolean)(using MutationP): C = {
      val CausalStore(dm, _) = current
      val removedDots = dm.collect {
        case (k, v) if cond(k) => v
      }.foldLeft(DotSet.empty)(_ union _)

      deltaState[E].make(
        cc = CausalContext.fromSet(removedDots)
      )
    }

    def clear()(using MutationP): C = {
      val CausalStore(dm, _) = current
      deltaState[E].make(
        cc = CausalContext.fromSet(DotMap[E, DotSet].dots(dm))
      )
    }

  }

  private class DeltaStateFactory[E] {
    val bottom: AWSet[E] = UIJDLattice[AWSet[E]].bottom

    def make(
        dm: DotMap[E, DotSet] = bottom.store,
        cc: C = bottom.context
    ): AWSet[E] = CausalStore(dm, cc)
  }

  private def deltaState[E]: DeltaStateFactory[E] = new DeltaStateFactory[E]

}
