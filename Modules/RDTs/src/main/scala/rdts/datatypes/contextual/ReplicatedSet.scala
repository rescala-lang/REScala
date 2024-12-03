package rdts.datatypes.contextual

import rdts.base.{Bottom, Decompose, Lattice, LocalUid}
import rdts.dotted.*
import rdts.dotted.HasDots.mapInstance
import rdts.time.{Dot, Dots}

/** A set that allows deletes.
  * Each unique element tracks the dots of when it was inserted.
  * Removals do not override concurrent inserts.
  */
case class ReplicatedSet[E](inner: Map[E, Dots]) derives Lattice, Decompose {

  type Delta = Dotted[ReplicatedSet[E]]

  def elements: Set[E] = inner.keySet

  def contains(elem: E): Boolean = inner.contains(elem)

  def add(using LocalUid)(e: E)(using context: Dots): Delta = {
    val nextDot = context.nextDot(LocalUid.replicaId)
    val v: Dots = inner.getOrElse(e, Dots.empty)

    deltaState(
      dm = Map(e -> Dots.single(nextDot)),
      cc = v `add` nextDot
    )
  }

  def addAll(using LocalUid)(elems: Iterable[E])(using context: Dots): Delta = {
    val nextCounter = context.nextTime(LocalUid.replicaId)
    val nextDots    = Dots.from((nextCounter until nextCounter + elems.size).map(Dot(LocalUid.replicaId, _)))

    val ccontextSet = elems.foldLeft(nextDots) {
      case (dots, e) => inner.get(e) match {
          case Some(ds) => dots `union` ds
          case None     => dots
        }
    }

    deltaState(
      dm = (elems zip nextDots.iterator.map(dot => Dots.single(dot))).toMap,
      cc = ccontextSet
    )
  }

  def remove(e: E): Delta = {
    val v = inner.getOrElse(e, Dots.empty)

    deltaState(v)
  }

  def removeAll(elems: Iterable[E]): Delta = {
    val dotsToRemove = elems.foldLeft(Dots.empty) {
      case (dots, e) => inner.get(e) match {
          case Some(ds) => dots `union` ds
          case None     => dots
        }
    }

    deltaState(dotsToRemove)
  }

  def removeBy(cond: E => Boolean): Delta = {
    val removedDots = inner.collect {
      case (k, v) if cond(k) => v
    }.foldLeft(Dots.empty)(_ `union` _)

    deltaState(
      cc = removedDots
    )
  }

  def clear(): Delta = { deltaState(inner.dots) }

  private def deltaState(
      dm: Map[E, Dots],
      cc: Dots
  ): Delta = Dotted(ReplicatedSet(dm), cc)

  private def deltaState(cc: Dots): Delta = deltaState(Map.empty, cc)
}

object ReplicatedSet {

  def empty[E]: ReplicatedSet[E] = ReplicatedSet(Map.empty)

  given bottom[E]: Bottom[ReplicatedSet[E]]   = Bottom.provide(empty)
  given lattice[E]: Lattice[ReplicatedSet[E]] = Lattice.derived
  given decompose[E]: Decompose[ReplicatedSet[E]] = Decompose.derived
  given hasDots[E]: HasDots[ReplicatedSet[E]] = HasDots.derived

}
