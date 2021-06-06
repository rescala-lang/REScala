package rescala.extra.lattices.delta.crdt.basic

import rescala.extra.lattices.delta.{CRDTInterface, Delta, UIJDLattice}

trait BasicCRDT[State, Wrapper] extends CRDTInterface[State, Wrapper] {
  protected val antiEntropy: AntiEntropy[State]

  override protected val replicaID: String = antiEntropy.replicaID

  protected def copy(state: State = state): Wrapper

  override def applyDelta(delta: Delta[State])(implicit u: UIJDLattice[State]): Wrapper = delta match {
    case Delta(origin, deltaState) =>
      UIJDLattice[State].diff(state, deltaState) match {
        case Some(stateDiff) =>
          val stateMerged = UIJDLattice[State].merge(state, stateDiff)
          antiEntropy.recordChange(Delta(origin, stateDiff), stateMerged)
          copy(state = stateMerged)
        case None => this.asInstanceOf[Wrapper]
      }
  }

  def processReceivedDeltas()(implicit u: UIJDLattice[State]): Wrapper = antiEntropy.getReceivedDeltas.foldLeft(this) {
    (crdt, delta) => crdt.applyDelta(delta).asInstanceOf[BasicCRDT[State, Wrapper]]
  }.asInstanceOf[Wrapper]
}
