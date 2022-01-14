package rescala.extra.lattices.delta.crdt.reactive

import kofre.decompose.{CRDTInterface, Delta, UIJDLattice}

/** ReactiveCRDTs are Delta CRDTs that store applied deltas in their deltaBuffer attribute. Middleware should regularly
  * take these deltas and ship them to other replicas, using applyDelta to apply them on the remote state. After deltas
  * have been read and propagated by the middleware, it should call resetDeltaBuffer to empty the deltaBuffer.
  */
trait ReactiveCRDT[State, Wrapper] extends CRDTInterface[State, Wrapper] {
  val deltaBuffer: List[Delta[State]]

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
