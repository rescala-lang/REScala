package kofre.datatypes.contextual

import kofre.base.{Bottom, Lattice}
import kofre.dotted.*
import kofre.syntax.{OpsSyntaxHelper, ReplicaId}
import kofre.time.{Dot, Dots}
import kofre.dotted.HasDots.mapInstance

/** An AddWinsSet (Add-Wins Set) is a Delta CRDT modeling a set.
  *
  * When an element is concurrently added and removed/cleared from the set then the add operation wins, i.e. the resulting set contains the element.
  */
case class ReplicatedSet[E](inner: Map[E, DotSet])

object ReplicatedSet {

  def empty[E]: ReplicatedSet[E] = ReplicatedSet(Map.empty)

  given bottom[E]: Bottom[ReplicatedSet[E]] with { override def empty: ReplicatedSet[E] = ReplicatedSet.empty }

  given lattice[E]: Lattice[ReplicatedSet[E]]         = Lattice.derived
  given asCausalContext[E]: HasDots[ReplicatedSet[E]] = HasDots.derived

  extension [C, E](container: C)
    def addWinsSet: syntax[C, E] = syntax(container)

  implicit class syntax[C, E](container: C) extends OpsSyntaxHelper[C, ReplicatedSet[E]](container) {

    def elements(using IsQuery): Set[E] = current.inner.keySet

    def contains(using IsQuery)(elem: E): Boolean = current.inner.contains(elem)

    def addElem(using rid: ReplicaId, dots: Dots, isQuery: IsQuery)(e: E): Dotted[ReplicatedSet[E]] =
      Dotted(current, dots).add(using rid)(e)(using Dotted.syntaxPermissions)

    def removeElem(using rid: ReplicaId, dots: Dots, isQuery: IsQuery)(e: E): Dotted[ReplicatedSet[E]] =
        Dotted(current, dots).remove(using Dotted.syntaxPermissions, Dotted.syntaxPermissions)(e)

    def add(using ReplicaId)(e: E): CausalMutator = {
      val dm        = current.inner
      val nextDot   = context.nextDot(replicaId)
      val v: DotSet = dm.getOrElse(e, DotSet.empty)

      deltaState(
        dm = Map(e -> DotSet(Dots.single(nextDot))),
        cc = v.dots add nextDot
      ).mutator
    }

    def addAll(using ReplicaId, IsCausalMutator)(elems: Iterable[E]): C = {
      val dm          = current.inner
      val cc          = context
      val nextCounter = cc.nextTime(replicaId)
      val nextDots    = Dots.from((nextCounter until nextCounter + elems.size).map(Dot(replicaId, _)))

      val ccontextSet = elems.foldLeft(nextDots) {
        case (dots, e) => dm.get(e) match {
            case Some(ds) => dots union ds.dots
            case None     => dots
          }
      }

      deltaState(
        dm = (elems zip nextDots.iterator.map(dot => DotSet(Dots.single(dot)))).toMap,
        cc = ccontextSet
      ).mutator
    }

    def remove(using IsQuery, IsCausalMutator)(e: E): C = {
      val dm = current.inner
      val v  = dm.getOrElse(e, DotSet.empty)

      deltaState(
        v.dots
      ).mutator
    }

    def removeAll(elems: Iterable[E])(using IsQuery, IsCausalMutator): C = {
      val dm = current.inner
      val dotsToRemove = elems.foldLeft(Dots.empty) {
        case (dots, e) => dm.get(e) match {
            case Some(ds) => dots union ds.dots
            case None     => dots
          }
      }

      deltaState(
        dotsToRemove
      ).mutator
    }

    def removeBy(cond: E => Boolean)(using IsQuery, IsCausalMutator): C = {
      val dm = current.inner
      val removedDots = dm.collect {
        case (k, v) if cond(k) => v
      }.foldLeft(Dots.empty)(_ union _.dots)

      deltaState(
        removedDots
      ).mutator
    }

    def clear()(using IsQuery, IsCausalMutator): C = {
      val dm = current.inner
      deltaState(
        dm.dots
      ).mutator
    }

  }

  private def deltaState[E](
      dm: Map[E, DotSet],
      cc: Dots
  ): Dotted[ReplicatedSet[E]] = Dotted(ReplicatedSet(dm), cc)

  private def deltaState[E](cc: Dots): Dotted[ReplicatedSet[E]] = deltaState(Map.empty, cc)

}
