package rescala.extra.lattices.delta.crdt.basic

import kofre.decompose.{CRDTInterface, Delta, UIJDLattice}
import rescala.extra.replication.AntiEntropy

/** BasicCRDTs are Delta CRDTs that use [[AntiEntropy]] and [[Network]] as Middleware for exchanging deltas between replicas.
  * They cannot actually be used on multiple connected replicas, but are useful for locally testing the behavior of
  * Delta CRDTs.
  *
  * Generated deltas are automatically propagated to the registered [[AntiEntropy]] instance, but to apply deltas received
  * by the AntiEntropy instance you need to explicitly call processReceivedDeltas on the CRDT.
  */
trait BasicCRDT[State, Wrapper] extends CRDTInterface[State, Wrapper] {
  protected val antiEntropy: AntiEntropy[State]

  override val replicaID: String = antiEntropy.replicaID

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
