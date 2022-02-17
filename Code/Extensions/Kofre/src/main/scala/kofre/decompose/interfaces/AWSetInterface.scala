package kofre.decompose.interfaces

import kofre.decompose.*
import kofre.decompose.CRDTInterface.{DeltaMutator, DeltaQuery}
import kofre.decompose.DotStore.*

object AWSetInterface {
  type State[E, C] = Causal[DotMap[E, DotSet], C]

  trait AWSetCompanion {
    type State[E, C] = AWSetInterface.State[E, C]
    type Embedded[E] = DotMap[E, DotSet]
  }

  private class DeltaStateFactory[E, C: CContext] {
    val bottom: State[E, C] = UIJDLattice[State[E, C]].bottom

    def make(
        dm: DotMap[E, DotSet] = bottom.dotStore,
        cc: C = bottom.cc
    ): State[E, C] = Causal(dm, cc)
  }

  private def deltaState[E, C: CContext]: DeltaStateFactory[E, C] = new DeltaStateFactory[E, C]

  def elements[E, C: CContext]: DeltaQuery[State[E, C], Set[E]] = {
    case Causal(dm, _) => dm.keySet
  }

  def add[E, C: CContext](e: E): DeltaMutator[State[E, C]] = {
    case (replicaID, Causal(dm, cc)) =>
      val nextDot = CContext[C].nextDot(cc, replicaID)
      val v       = dm.getOrElse(e, DotSet.empty)

      deltaState[E, C].make(
        dm = DotMap[E, DotSet].empty.updated(e, Set(nextDot)),
        cc = CContext[C].fromSet(v + nextDot)
      )
  }

  def addAll[E, C: CContext](elems: Iterable[E]): DeltaMutator[State[E, C]] = {
    case (replicaID, Causal(dm, cc)) =>
      val nextCounter = CContext[C].nextDot(cc, replicaID).counter
      val nextDots    = (nextCounter until nextCounter + elems.size).toSet.map(Dot(replicaID, _))

      val ccontextSet = elems.foldLeft(nextDots) {
        case (dots, e) => dm.get(e) match {
            case Some(ds) => dots union ds
            case None     => dots
          }
      }

      deltaState[E, C].make(
        dm = (elems zip nextDots.map(Set(_))).toMap,
        cc = CContext[C].fromSet(ccontextSet)
      )
  }

  def remove[E, C: CContext](e: E): DeltaMutator[State[E, C]] = {
    case (_, Causal(dm, _)) =>
      val v = dm.getOrElse(e, DotSet.empty)

      deltaState[E, C].make(
        cc = CContext[C].fromSet(v)
      )
  }

  def removeAll[E, C: CContext](elems: Iterable[E]): DeltaMutator[State[E, C]] = {
    case (_, Causal(dm, _)) =>
      val dotsToRemove = elems.foldLeft(Set.empty[Dot]) {
        case (dots, e) => dm.get(e) match {
            case Some(ds) => dots union ds
            case None     => dots
          }
      }

      deltaState[E, C].make(
        cc = CContext[C].fromSet(dotsToRemove)
      )
  }

  def removeBy[E, C: CContext](cond: E => Boolean): DeltaMutator[State[E, C]] = {
    case (_, Causal(dm, _)) =>
      val removedDots = dm.collect {
        case (k, v) if cond(k) => v
      }.foldLeft(DotSet.empty)(_ union _)

      deltaState[E, C].make(
        cc = CContext[C].fromSet(removedDots)
      )
  }

  def clear[E, C: CContext](): DeltaMutator[State[E, C]] = {
    case (_, Causal(dm, _)) =>
      deltaState[E, C].make(
        cc = CContext[C].fromSet(DotMap[E, DotSet].dots(dm))
      )
  }
}

/** An AWSet (Add-Wins Set) is a Delta CRDT modeling a set.
  *
  * When an element is concurrently added and removed/cleared from the set then the add operation wins, i.e. the resulting set contains the element.
  */
abstract class AWSetInterface[E, C: CContext, Wrapper] extends CRDTInterface[AWSetInterface.State[E, C], Wrapper] {
  def elements: Set[E] = query(AWSetInterface.elements)

  def add(e: E): Wrapper = mutate(AWSetInterface.add(e))

  def addAll(elems: Iterable[E]): Wrapper = mutate(AWSetInterface.addAll(elems))

  def remove(e: E): Wrapper = mutate(AWSetInterface.remove(e))

  def removeAll(elems: Iterable[E]): Wrapper = mutate(AWSetInterface.removeAll(elems))

  def removeBy(cond: E => Boolean): Wrapper = mutate(AWSetInterface.removeBy(cond))

  def clear(): Wrapper = mutate(AWSetInterface.clear())
}
