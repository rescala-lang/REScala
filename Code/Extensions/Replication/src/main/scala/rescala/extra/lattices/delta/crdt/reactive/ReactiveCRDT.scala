package rescala.extra.lattices.delta.crdt.reactive

import rescala.extra.lattices.delta.{CRDTInterface, Delta, UIJDLattice}

trait ReactiveCRDT[State, Wrapper] extends CRDTInterface[State, Wrapper] {
  protected[rescala] val deltaBuffer: List[Delta[State]]

  protected def copy(state: State = state, deltaBuffer: List[Delta[State]] = deltaBuffer): Wrapper

  override def applyDelta(delta: Delta[State])(implicit u: UIJDLattice[State]): Wrapper = delta match {
    case Delta(origin, deltaState) =>
      UIJDLattice[State].diff(state, deltaState) match {
        case Some(stateDiff) =>
          val stateMerged = UIJDLattice[State].merge(state, stateDiff)
          copy(state = stateMerged, deltaBuffer = Delta(origin, stateDiff) :: deltaBuffer)
        case None => this.asInstanceOf[Wrapper]
      }
  }

  def resetDeltaBuffer(): Wrapper = copy(deltaBuffer = List())
}
