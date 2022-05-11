package kofre.predef

import kofre.base.DecomposeLattice
import kofre.causality.{CausalContext, Dot}
import kofre.contextual.{AsCausalContext, ContextDecompose, WithContext}
import kofre.contextual.ContextDecompose.*
import kofre.decompose.*
import kofre.predef.AddWinsSet
import kofre.predef.AddWinsSet.Embedded
import kofre.syntax.OpsSyntaxHelper

/** An AddWinsSet (Add-Wins Set) is a Delta CRDT modeling a set.
  *
  * When an element is concurrently added and removed/cleared from the set then the add operation wins, i.e. the resulting set contains the element.
  */
case class AddWinsSet[E](inner: Map[E, CausalContext])


object AddWinsSet {
  type Embedded[E] = Map[E, CausalContext]

  def empty[E]: AddWinsSet[E] = AddWinsSet(Map.empty)

  given contextDecompose[E]: ContextDecompose[AddWinsSet[E]] = ContextDecompose.product1ContextDecompose

  implicit class AWSetSyntax[C, E](container: C) extends OpsSyntaxHelper[C, AddWinsSet[E]](container) {

    def elements(using QueryP): Set[E] = current.inner.keySet

    def contains(elem: E)(using QueryP): Boolean = current.inner.contains(elem)

    def add(e: E)(using CausalP, CausalMutationP, QueryP, IdentifierP): C = {
      val dm      = current.inner
      val cc      = context
      val nextDot = cc.max(replicaID).fold(Dot(replicaID, 0))(_.advance)
      val v       = dm.getOrElse(e, CausalContext.empty)

      deltaState[E].make(
        dm = Map(e -> CausalContext.single(nextDot)),
        cc = v add nextDot
      ).mutator
    }

    def addAll(elems: Iterable[E])(using IdentifierP, CausalP, CausalMutationP, QueryP): C = {
      val dm          = current.inner
      val cc          = context
      val nextCounter = cc.nextTime(replicaID)
      val nextDots    = CausalContext.fromSet((nextCounter until nextCounter + elems.size).map(Dot(replicaID, _)))

      val ccontextSet = elems.foldLeft(nextDots) {
        case (dots, e) => dm.get(e) match {
            case Some(ds) => dots union ds
            case None     => dots
          }
      }

      deltaState[E].make(
        dm = (elems zip nextDots.iterator.map(CausalContext.single)).toMap,
        cc = ccontextSet
      ).mutator
    }

    def remove(e: E)(using QueryP, CausalMutationP): C = {
      val dm = current.inner
      val v  = dm.getOrElse(e, CausalContext.empty)

      deltaState[E].make(
        cc = v
      ).mutator
    }

    def removeAll(elems: Iterable[E])(using QueryP, CausalMutationP): C = {
      val dm = current.inner
      val dotsToRemove = elems.foldLeft(CausalContext.empty) {
        case (dots, e) => dm.get(e) match {
            case Some(ds) => dots union ds
            case None     => dots
          }
      }

      deltaState[E].make(
        cc = dotsToRemove
      ).mutator
    }

    def removeBy(cond: E => Boolean)(using QueryP, CausalMutationP): C = {
      val dm = current.inner
      val removedDots = dm.collect {
        case (k, v) if cond(k) => v
      }.foldLeft(CausalContext.empty)(_ union _)

      deltaState[E].make(
        cc = removedDots
      ).mutator
    }

    def clear()(using QueryP, CausalMutationP): C = {
      val dm = current.inner
      deltaState[E].make(
        cc = AsCausalContext.DotMapInstance.dots(dm)
      ).mutator
    }

  }

  private class DeltaStateFactory[E] {

    def make(
        dm: Map[E, CausalContext] = Map.empty,
        cc: CausalContext = CausalContext.empty
    ): WithContext[AddWinsSet[E]] = WithContext(AddWinsSet(dm), cc)
  }

  private def deltaState[E]: DeltaStateFactory[E] = new DeltaStateFactory[E]

}
