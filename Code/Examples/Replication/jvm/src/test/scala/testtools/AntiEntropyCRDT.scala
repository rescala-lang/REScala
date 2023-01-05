package testtools

import kofre.base.DecomposeLattice
import kofre.decompose.containers.{AntiEntropy, CRDTInterface, Network}
import kofre.dotted.{Dotted, DottedDecompose, DottedLattice}
import kofre.syntax.{ArdtOpsContains, DottedName, PermCausal, PermCausalMutate, PermIdMutate}
import kofre.time.Dots

/** BasicCRDTs are Delta CRDTs that use [[AntiEntropy]] and [[Network]] as Middleware for exchanging deltas between replicas.
  * They cannot actually be used on multiple connected replicas, but are useful for locally testing the behavior of
  * Delta CRDTs.
  *
  * Generated deltas are automatically propagated to the registered [[AntiEntropy]] instance, but to apply deltas received
  * by the AntiEntropy instance you need to explicitly call processReceivedDeltas on the CRDT.
  */
class AntiEntropyCRDT[State](
    protected val antiEntropy: AntiEntropy[State]
) extends CRDTInterface[State, AntiEntropyCRDT[State]] {
  override val replicaID: String = antiEntropy.replicaID

  override def state: Dotted[State] = antiEntropy.state

  override def applyDelta(delta: DottedName[State])(using DecomposeLattice[Dotted[State]]): AntiEntropyCRDT[State] =
    delta match {
      case DottedName(origin, deltaCtx) =>
        DecomposeLattice[Dotted[State]].diff(state, deltaCtx) match {
          case Some(stateDiff) =>
            val stateMerged = DecomposeLattice[Dotted[State]].merge(state, stateDiff)
            antiEntropy.recordChange(DottedName(origin, stateDiff), stateMerged)
          case None =>
        }
        this
    }

  def processReceivedDeltas()(implicit u: DecomposeLattice[Dotted[State]]): AntiEntropyCRDT[State] =
    antiEntropy.getReceivedDeltas.foldLeft(this) {
      (crdt, delta) => crdt.applyDelta(delta)
    }
}

object AntiEntropyCRDT {

  given antiEntropyContains[State]: ArdtOpsContains[AntiEntropyCRDT[State], State] = new {}

  given allPermissions[L: DottedDecompose]
      : (PermIdMutate[AntiEntropyCRDT[L], L] & PermCausalMutate[AntiEntropyCRDT[L], L]) =
    CRDTInterface.dottedPermissions

  /** Creates a new PNCounter instance
    *
    * @param antiEntropy AntiEntropy instance used for exchanging deltas with other replicas
    */
  def apply[State](antiEntropy: AntiEntropy[State]): AntiEntropyCRDT[State] =
    new AntiEntropyCRDT(antiEntropy)
}
