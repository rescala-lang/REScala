package kofre.decompose.containers

import kofre.decompose.{Delta, UIJDLattice}
import kofre.syntax.{AllPermissionsCtx, ArdtOpsContains}

/** BasicCRDTs are Delta CRDTs that use [[JsoniterAntiEntropy]] and [[Network]] as Middleware for exchanging deltas between replicas.
  * They cannot actually be used on multiple connected replicas, but are useful for locally testing the behavior of
  * Delta CRDTs.
  *
  * Generated deltas are automatically propagated to the registered [[JsoniterAntiEntropy]] instance, but to apply deltas received
  * by the AntiEntropy instance you need to explicitly call processReceivedDeltas on the CRDT.
  */
class AntiEntropyCRDT[State](
    val state: State,
    protected val antiEntropy: AntiEntropy[State]
) extends CRDTInterface[State, AntiEntropyCRDT[State]] {
  override val replicaID: String = antiEntropy.replicaID

  override def applyDelta(delta: Delta[State])(implicit u: UIJDLattice[State]): AntiEntropyCRDT[State] = delta match {
    case Delta(origin, deltaState) =>
      UIJDLattice[State].diff(state, deltaState) match {
        case Some(stateDiff) =>
          val stateMerged = UIJDLattice[State].merge(state, stateDiff)
          antiEntropy.recordChange(Delta(origin, stateDiff), stateMerged)
          new AntiEntropyCRDT[State](stateMerged, antiEntropy)
        case None => this.asInstanceOf[AntiEntropyCRDT[State]]
      }
  }

  def processReceivedDeltas()(implicit u: UIJDLattice[State]): AntiEntropyCRDT[State] = antiEntropy.getReceivedDeltas.foldLeft(this) {
    (crdt, delta) => crdt.applyDelta(delta)
  }
}

object AntiEntropyCRDT {

  implicit def containesRelation[State]: ArdtOpsContains[AntiEntropyCRDT[State], State] =
    new ArdtOpsContains[AntiEntropyCRDT[State], State] {}

  implicit def antiEntropyPermissions[L: UIJDLattice]: AllPermissionsCtx[AntiEntropyCRDT[L], L] =
    CRDTInterface.crdtInterfaceContextPermissions[L, AntiEntropyCRDT[L]]

  /** Creates a new PNCounter instance
    *
    * @param antiEntropy AntiEntropy instance used for exchanging deltas with other replicas
    */
  def apply[State: UIJDLattice](antiEntropy: AntiEntropy[State]): AntiEntropyCRDT[State] =
    new AntiEntropyCRDT(UIJDLattice[State].empty, antiEntropy)
}
