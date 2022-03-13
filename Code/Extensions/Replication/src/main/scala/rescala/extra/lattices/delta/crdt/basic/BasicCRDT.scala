package rescala.extra.lattices.delta.crdt.basic

import kofre.decompose.{CRDTInterface, Delta, UIJDLattice}
import kofre.syntax.AllPermissionsCtx
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

/** [[BasicCRDT Basic]] implementation
  *
  * Instead of the class constructor, you should use the apply method of the companion object to create new instances.
  */
class AntiEntropyCRDT[State](
    val state: State,
    protected val antiEntropy: AntiEntropy[State]
) extends BasicCRDT[State, AntiEntropyCRDT[State]] {

  override protected def copy(state: State): AntiEntropyCRDT[State] = new AntiEntropyCRDT[State](state, antiEntropy)
}

object AntiEntropyCRDT {

  implicit def antiEntropyPermissions[L: UIJDLattice]: AllPermissionsCtx[AntiEntropyCRDT[L], L] =
    CRDTInterface.crdtInterfaceContextPermissions[L, AntiEntropyCRDT[L]]

  /** Creates a new PNCounter instance
    *
    * @param antiEntropy AntiEntropy instance used for exchanging deltas with other replicas
    */
  def apply[State: UIJDLattice](antiEntropy: AntiEntropy[State]): AntiEntropyCRDT[State] =
    new AntiEntropyCRDT(UIJDLattice[State].bottom, antiEntropy)
}
