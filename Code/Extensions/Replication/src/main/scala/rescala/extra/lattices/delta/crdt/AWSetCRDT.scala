package rescala.extra.lattices.delta.crdt

import rescala.extra.lattices.delta.DotStore._
import rescala.extra.lattices.delta._

object AWSetCRDT {
  type State[E, C] = Causal[DotMap[E, DotSet], C]

  private def deltaState[E, C: CContext](
      dm: Option[DotMap[E, DotSet]] = None,
      cc: Option[C]
  ): State[E, C] = {
    val bottom = UIJDLattice[State[E, C]].bottom

    Causal(
      dm.getOrElse(bottom.dotStore),
      cc.getOrElse(bottom.cc)
    )
  }

  def elements[E, C: CContext]: DeltaQuery[State[E, C], Set[E]] = {
    case Causal(dm, _) => dm.keySet
  }

  def add[E, C: CContext](e: E): DeltaMutator[State[E, C]] = {
    case (replicaID, Causal(dm, cc)) =>
      val nextDot = CContext[C].nextDot(cc, replicaID)
      val v       = dm.getOrElse(e, DotSet.empty)

      deltaState(
        dm = Some(DotMap[E, DotSet].empty.updated(e, Set(nextDot))),
        cc = Some(CContext[C].fromSet(v + nextDot))
      )
  }

  def remove[E, C: CContext](e: E): DeltaMutator[State[E, C]] = {
    case (_, Causal(dm, _)) =>
      val v = dm.getOrElse(e, DotSet.empty)

      deltaState(
        cc = Some(CContext[C].fromSet(v))
      )
  }

  def removeBy[E, C: CContext](cond: E => Boolean): DeltaMutator[State[E, C]] = {
    case (_, Causal(dm, _)) =>
      val removedDots = dm.collect {
        case (k, v) if cond(k) => v
      }.foldLeft(DotSet.empty)(_ union _)

      deltaState(
        cc = Some(CContext[C].fromSet(removedDots))
      )
  }

  def clear[E, C: CContext](): DeltaMutator[State[E, C]] = {
    case (_, Causal(dm, _)) =>
      deltaState(
        cc = Some(CContext[C].fromSet(DotMap[E, DotSet].dots(dm)))
      )
  }
}
