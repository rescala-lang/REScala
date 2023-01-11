package kofre.datatypes

import kofre.base.{Bottom, DecomposeLattice}
import kofre.datatypes.AddWinsSet
import kofre.dotted.DottedDecompose.*
import kofre.dotted.{DotMap, DotSet, Dotted, DottedDecompose, HasDots}
import kofre.syntax.OpsSyntaxHelper
import kofre.time.{Dot, Dots}

/** An AddWinsSet (Add-Wins Set) is a Delta CRDT modeling a set.
  *
  * When an element is concurrently added and removed/cleared from the set then the add operation wins, i.e. the resulting set contains the element.
  */
case class AddWinsSet[E](inner: DotMap[E, DotSet])

object AddWinsSet {

  def empty[E]: AddWinsSet[E] = AddWinsSet(DotMap.empty)

  given bottom[E]: Bottom[AddWinsSet[E]] with { override def empty: AddWinsSet[E] = AddWinsSet.empty }

  given contextDecompose[E]: DottedDecompose[AddWinsSet[E]] = DottedDecompose.derived
  given asCausalContext[E]: HasDots[AddWinsSet[E]]          = HasDots.derived

  extension[C, E] (container: C)
    def addWinsSet: syntax[C, E] = syntax(container)

  implicit class syntax[C, E](container: C) extends OpsSyntaxHelper[C, AddWinsSet[E]](container) {

    def elements(using QueryP): Set[E] = current.inner.keySet

    def contains(using QueryP)(elem: E): Boolean = current.inner.contains(elem)

    def add(e: E)(using CausalP, CausalMutationP, QueryP, IdentifierP): C = {
      val dm        = current.inner
      val cc        = context
      val nextDot   = cc.max(replicaID).fold(Dot(replicaID, 0))(_.advance)
      val v: DotSet = dm.getOrElse(e, DotSet.empty)

      deltaState[E].make(
        dm = DotMap(Map(e -> DotSet(Dots.single(nextDot)))),
        cc = v.repr add nextDot
      ).mutator
    }

    def addAll(elems: Iterable[E])(using IdentifierP, CausalP, CausalMutationP, QueryP): C = {
      val dm          = current.inner
      val cc          = context
      val nextCounter = cc.nextTime(replicaID)
      val nextDots    = Dots.from((nextCounter until nextCounter + elems.size).map(Dot(replicaID, _)))

      val ccontextSet = elems.foldLeft(nextDots) {
        case (dots, e) => dm.get(e) match {
            case Some(ds) => dots union ds.repr
            case None     => dots
          }
      }

      deltaState[E].make(
        dm = DotMap((elems zip nextDots.iterator.map(dot => DotSet(Dots.single(dot)))).toMap),
        cc = ccontextSet
      ).mutator
    }

    def remove(using QueryP, CausalMutationP)(e: E): C = {
      val dm = current.inner
      val v  = dm.getOrElse(e, DotSet.empty)

      deltaState[E].make(
        cc = v.repr
      ).mutator
    }

    def removeAll(elems: Iterable[E])(using QueryP, CausalMutationP): C = {
      val dm = current.inner
      val dotsToRemove = elems.foldLeft(Dots.empty) {
        case (dots, e) => dm.get(e) match {
            case Some(ds) => dots union ds.repr
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
      }.foldLeft(Dots.empty)(_ union _.repr)

      deltaState[E].make(
        cc = removedDots
      ).mutator
    }

    def clear()(using QueryP, CausalMutationP): C = {
      val dm = current.inner
      deltaState[E].make(
        cc = dm.dots
      ).mutator
    }

  }

  private class DeltaStateFactory[E] {

    def make(
        dm: DotMap[E, DotSet] = DotMap.empty,
        cc: Dots = Dots.empty
    ): Dotted[AddWinsSet[E]] = Dotted(AddWinsSet(dm), cc)
  }

  private def deltaState[E]: DeltaStateFactory[E] = new DeltaStateFactory[E]

}
