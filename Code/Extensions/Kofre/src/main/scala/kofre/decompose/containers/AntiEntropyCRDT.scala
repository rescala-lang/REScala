package kofre.decompose.containers

import kofre.base.DecomposeLattice
import kofre.causality.CausalContext
import kofre.contextual.WithContext
import kofre.decompose.Delta
import kofre.syntax.{ArdtOpsContains, PermCausal, PermCausalMutate, PermIdMutate}

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

  override def context: CausalContext = antiEntropy.context

  override def applyDelta(delta: Delta[State])(implicit u: DecomposeLattice[State]): AntiEntropyCRDT[State] =
    delta match {
      case Delta(origin, context, deltaState) =>
        DecomposeLattice[State].diff(state, deltaState) match {
          case Some(stateDiff) =>
            val stateMerged = DecomposeLattice[State].merge(state, stateDiff)
            antiEntropy.recordChange(Delta(origin, context, stateDiff), stateMerged)
            new AntiEntropyCRDT[State](stateMerged, antiEntropy)
          case None => this.asInstanceOf[AntiEntropyCRDT[State]]
        }
    }

  def processReceivedDeltas()(implicit u: DecomposeLattice[State]): AntiEntropyCRDT[State] =
    antiEntropy.getReceivedDeltas.foldLeft(this) {
      (crdt, delta) => crdt.applyDelta(delta)
    }
}

object AntiEntropyCRDT {

  given containsRelation[State]: ArdtOpsContains[AntiEntropyCRDT[State], State] =
    new ArdtOpsContains[AntiEntropyCRDT[State], State] {}

  given antiEntropyPermissions[L: DecomposeLattice]: PermIdMutate[AntiEntropyCRDT[L], L] =
    CRDTInterface.crdtInterfaceContextPermissions[L, AntiEntropyCRDT[L]]

  given contextPermissions[L](using
      DecomposeLattice[L]
  ): (PermCausalMutate[AntiEntropyCRDT[L], L] with PermCausal[AntiEntropyCRDT[L]]) =
    CRDTInterface.contextPermissions

  /** Creates a new PNCounter instance
    *
    * @param antiEntropy AntiEntropy instance used for exchanging deltas with other replicas
    */
  def apply[State: DecomposeLattice](antiEntropy: AntiEntropy[State]): AntiEntropyCRDT[State] =
    new AntiEntropyCRDT(DecomposeLattice[State].empty, antiEntropy)
}
