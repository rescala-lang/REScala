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
class ReactiveDeltaCRDT[State](
    val state: State,
    val replicaID: String,
    val context: CausalContext,
    val deltaBuffer: List[Delta[State]]
) extends CRDTInterface[State, ReactiveDeltaCRDT[State]] {

  def copy(
      state: State = state,
      context: CausalContext = context,
      deltaBuffer: List[Delta[State]] = deltaBuffer
  ): ReactiveDeltaCRDT[State] =
    new ReactiveDeltaCRDT[State](state, replicaID, context, deltaBuffer)

  override def applyDelta(delta: Delta[State])(implicit u: DecomposeLattice[State]): ReactiveDeltaCRDT[State] =
    delta match {
      case Delta(origin, context, deltaState) =>
        DecomposeLattice[State].diff(state, deltaState) match {
          case Some(stateDiff) =>
            val stateMerged = DecomposeLattice[State].merge(state, stateDiff)
            copy(state = stateMerged, deltaBuffer = Delta(origin, context, stateDiff) :: deltaBuffer)
          case None => this.asInstanceOf[ReactiveDeltaCRDT[State]]
        }
    }

  def resetDeltaBuffer(): ReactiveDeltaCRDT[State] = copy(deltaBuffer = List())
}

object ReactiveDeltaCRDT {

  implicit def containsRelation[State]: ArdtOpsContains[ReactiveDeltaCRDT[State], State] =
    new ArdtOpsContains[ReactiveDeltaCRDT[State], State] {}

  implicit def reactiveDeltaCRDTPermissions[L: DecomposeLattice]: PermIdMutate[ReactiveDeltaCRDT[L], L] =
    CRDTInterface.crdtInterfaceContextPermissions[L, ReactiveDeltaCRDT[L]]

  implicit def contextPermissions[L](using
      DecomposeLattice[L]
  ): PermCausalMutate[ReactiveDeltaCRDT[L], L] with PermCausal[ReactiveDeltaCRDT[L]] = {
    type B = ReactiveDeltaCRDT[L]
    new PermCausalMutate[B, L] with PermCausal[B] {
      override def mutateContext(
          container: B,
          withContext: WithContext[L]
      ): B =
        container.applyDelta(Delta(container.replicaID, withContext.context, withContext.store))
      override def context(c: B): CausalContext = c.context
    }
  }

  /** Creates a new PNCounter instance
    *
    * @param replicaID Unique id of the replica that this instance is located on
    */
  def apply[State: DecomposeLattice](replicaID: String): ReactiveDeltaCRDT[State] =
    new ReactiveDeltaCRDT[State](DecomposeLattice[State].empty, replicaID, CausalContext.empty, List())
}
