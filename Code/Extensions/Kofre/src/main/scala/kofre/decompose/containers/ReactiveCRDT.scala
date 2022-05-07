package kofre.decompose.containers

import kofre.decompose.{Delta, DecomposeLattice}
import kofre.syntax.{AllPermissionsCtx, ArdtOpsContains}

/** ReactiveCRDTs are Delta CRDTs that store applied deltas in their deltaBuffer attribute. Middleware should regularly
  * take these deltas and ship them to other replicas, using applyDelta to apply them on the remote state. After deltas
  * have been read and propagated by the middleware, it should call resetDeltaBuffer to empty the deltaBuffer.
  */
trait ReactiveCRDT[State, Wrapper] extends CRDTInterface[State, Wrapper] {
  val deltaBuffer: List[Delta[State]]

  protected def copy(state: State = state, deltaBuffer: List[Delta[State]] = deltaBuffer): Wrapper

  override def applyDelta(delta: Delta[State])(implicit u: DecomposeLattice[State]): Wrapper = delta match {
    case Delta(origin, deltaState) =>
      DecomposeLattice[State].diff(state, deltaState) match {
        case Some(stateDiff) =>
          val stateMerged = DecomposeLattice[State].merge(state, stateDiff)
          copy(state = stateMerged, deltaBuffer = Delta(origin, stateDiff) :: deltaBuffer)
        case None => this.asInstanceOf[Wrapper]
      }
  }

  def resetDeltaBuffer(): Wrapper = copy(deltaBuffer = List())
}

/** [[ReactiveCRDT Reactive]] implementation
  *
  * Instead of the class constructor, you should use the apply method of the companion object to create new instances.
  */
class ReactiveDeltaCRDT[State](
    val state: State,
    val replicaID: String,
    val deltaBuffer: List[Delta[State]]
) extends ReactiveCRDT[State, ReactiveDeltaCRDT[State]] {

  override protected def copy(state: State, deltaBuffer: List[Delta[State]]): ReactiveDeltaCRDT[State] =
    new ReactiveDeltaCRDT[State](state, replicaID, deltaBuffer)
}

object ReactiveDeltaCRDT {

  implicit def containesRelation[State]: ArdtOpsContains[ReactiveDeltaCRDT[State], State] = new ArdtOpsContains[ReactiveDeltaCRDT[State], State] {}

  implicit def reactiveDeltaCRDTPermissions[L: DecomposeLattice]: AllPermissionsCtx[ReactiveDeltaCRDT[L], L] =
    CRDTInterface.crdtInterfaceContextPermissions[L, ReactiveDeltaCRDT[L]]

  /** Creates a new PNCounter instance
    *
    * @param replicaID Unique id of the replica that this instance is located on
    */
  def apply[State: DecomposeLattice](replicaID: String): ReactiveDeltaCRDT[State] =
    new ReactiveDeltaCRDT[State](DecomposeLattice[State].empty, replicaID, List())
}
