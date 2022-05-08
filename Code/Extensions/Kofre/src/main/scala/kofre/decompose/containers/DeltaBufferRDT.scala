package kofre.decompose.containers

import kofre.base.DecomposeLattice
import kofre.causality.CausalContext
import kofre.contextual.WithContext
import kofre.decompose.Delta
import kofre.syntax.{ArdtOpsContains, PermCausal, PermCausalMutate, PermIdMutate, PermQuery}

/** ReactiveCRDTs are Delta CRDTs that store applied deltas in their deltaBuffer attribute. Middleware should regularly
  * take these deltas and ship them to other replicas, using applyDelta to apply them on the remote state. After deltas
  * have been read and propagated by the middleware, it should call resetDeltaBuffer to empty the deltaBuffer.
  */
class DeltaBufferRDT[State](
    val state: State,
    val replicaID: String,
    val context: CausalContext,
    val deltaBuffer: List[Delta[State]]
) extends CRDTInterface[State, DeltaBufferRDT[State]] {

  def copy(
      state: State = state,
      context: CausalContext = context,
      deltaBuffer: List[Delta[State]] = deltaBuffer
  ): DeltaBufferRDT[State] =
    new DeltaBufferRDT[State](state, replicaID, context, deltaBuffer)

  override def applyDelta(delta: Delta[State])(implicit u: DecomposeLattice[State]): DeltaBufferRDT[State] =
    delta match {
      case Delta(origin, context, deltaState) =>
        DecomposeLattice[State].diff(state, deltaState) match {
          case Some(stateDiff) =>
            val stateMerged = DecomposeLattice[State].merge(state, stateDiff)
            copy(
              state = stateMerged,
              context = this.context merged context,
              deltaBuffer = Delta(origin, context, stateDiff) :: deltaBuffer
            )
          case None => this.asInstanceOf[DeltaBufferRDT[State]]
        }
    }

  def resetDeltaBuffer(): DeltaBufferRDT[State] = copy(deltaBuffer = List())
}

object DeltaBufferRDT {

  implicit def containsRelation[State]: ArdtOpsContains[DeltaBufferRDT[State], State] =
    new ArdtOpsContains[DeltaBufferRDT[State], State] {}

  implicit def reactiveDeltaCRDTPermissions[L: DecomposeLattice]: PermIdMutate[DeltaBufferRDT[L], L] =
    CRDTInterface.crdtInterfaceContextPermissions[L, DeltaBufferRDT[L]]

  implicit def contextPermissions[L](using
      DecomposeLattice[L]
  ): PermCausalMutate[DeltaBufferRDT[L], L] with PermCausal[DeltaBufferRDT[L]] = CRDTInterface.contextPermissions

  /** Creates a new PNCounter instance
    *
    * @param replicaID Unique id of the replica that this instance is located on
    */
  def apply[State: DecomposeLattice](replicaID: String): DeltaBufferRDT[State] =
    new DeltaBufferRDT[State](DecomposeLattice[State].empty, replicaID, CausalContext.empty, List())
}
