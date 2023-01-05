package kofre.decompose.containers

import kofre.base.{Bottom, DecomposeLattice, Id}
import kofre.time.Dots
import kofre.dotted.{DottedDecompose, DottedLattice, Dotted}
import kofre.syntax.{ArdtOpsContains, PermCausal, PermCausalMutate, PermIdMutate, PermQuery, DottedName}

/** ReactiveCRDTs are Delta CRDTs that store applied deltas in their deltaBuffer attribute. Middleware should regularly
  * take these deltas and ship them to other replicas, using applyDelta to apply them on the remote state. After deltas
  * have been read and propagated by the middleware, it should call resetDeltaBuffer to empty the deltaBuffer.
  */
class DeltaBufferRDT[State](
    val state: Dotted[State],
    val replicaID: Id,
    val deltaBuffer: List[DottedName[State]]
) extends CRDTInterface[State, DeltaBufferRDT[State]] {

  def copy(
      state: Dotted[State] = state,
      deltaBuffer: List[DottedName[State]] = deltaBuffer
  ): DeltaBufferRDT[State] =
    new DeltaBufferRDT[State](state, replicaID, deltaBuffer)

  override def applyDelta(delta: DottedName[State])(implicit
      u: DecomposeLattice[Dotted[State]]
  ): DeltaBufferRDT[State] =
    delta match {
      case DottedName(origin, delta) =>
        DecomposeLattice[Dotted[State]].diff(state, delta) match {
          case Some(stateDiff) =>
            val stateMerged = DecomposeLattice[Dotted[State]].merge(state, stateDiff)
            copy(
              state = stateMerged,
              deltaBuffer = DottedName(origin, stateDiff) :: deltaBuffer
            )
          case None => this.asInstanceOf[DeltaBufferRDT[State]]
        }
    }

  def resetDeltaBuffer(): DeltaBufferRDT[State] = copy(deltaBuffer = List())
}

object DeltaBufferRDT {

  given contextPermissions[L: DottedDecompose]
      : (PermIdMutate[DeltaBufferRDT[L], L] & PermCausalMutate[DeltaBufferRDT[L], L]) = CRDTInterface.dottedPermissions

  given fullPermission[L: DecomposeLattice: Bottom]: PermIdMutate[DeltaBufferRDT[L], L] = CRDTInterface.fullPermissions

  /** Creates a new PNCounter instance
    *
    * @param replicaID Unique id of the replica that this instance is located on
    */
  def apply[State](replicaID: Id)(using bot: Bottom[Dotted[State]]): DeltaBufferRDT[State] =
    new DeltaBufferRDT[State](bot.empty, replicaID, List())

  def empty[State](replicaID: Id, init: State): DeltaBufferRDT[State] =
    new DeltaBufferRDT[State](Dotted(init), replicaID, List())

  def apply[State](replicaID: Id, init: State): DeltaBufferRDT[State] = empty(replicaID, init)
}
