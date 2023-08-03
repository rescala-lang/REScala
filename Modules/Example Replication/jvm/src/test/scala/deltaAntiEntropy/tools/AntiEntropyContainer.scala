package deltaAntiEntropy.tools

import kofre.base.Lattice
import kofre.dotted.{Dotted, DottedLattice}
import kofre.syntax.{PermCausalMutate, PermMutate}
import kofre.time.Dots
import kofre.base.Uid
import kofre.base.Uid.asId

/** BasicCRDTs are Delta CRDTs that use [[IAntiEntropy]] and [[Network]] as Middleware for exchanging deltas between replicas.
  * They cannot actually be used on multiple connected replicas, but are useful for locally testing the behavior of
  * Delta CRDTs.
  *
  * Generated deltas are automatically propagated to the registered [[IAntiEntropy]] instance, but to apply deltas received
  * by the AntiEntropy instance you need to explicitly call processReceivedDeltas on the CRDT.
  */
class AntiEntropyContainer[State](
    protected val antiEntropy: AntiEntropy[State]
) {
  val replicaID: Uid = antiEntropy.replicaID.asId

  def state: Dotted[State] = antiEntropy.state

  override def toString: String =
    s"AntiEntropy($replicaID, $state)"

  def applyDelta(delta: Named[Dotted[State]])(using Lattice[Dotted[State]]): AntiEntropyContainer[State] =
    delta match {
      case Named(origin, deltaCtx) =>
        Lattice[Dotted[State]].diff(state, deltaCtx) match {
          case Some(stateDiff) =>
            val stateMerged = Lattice[Dotted[State]].merge(state, stateDiff)
            antiEntropy.recordChange(Named(origin, stateDiff), stateMerged)
          case None =>
        }
        this
    }

  def processReceivedDeltas()(implicit u: Lattice[Dotted[State]]): AntiEntropyContainer[State] =
    antiEntropy.getReceivedDeltas.foldLeft(this) {
      (crdt, delta) => crdt.applyDelta(delta)
    }
}

object AntiEntropyContainer {

  given allPermissions[L: DottedLattice]
      : (PermMutate[AntiEntropyContainer[L], L] & PermCausalMutate[AntiEntropyContainer[L], L]) =
    new PermMutate[AntiEntropyContainer[L], L] with PermCausalMutate[AntiEntropyContainer[L], L] {
      override def mutate(c: AntiEntropyContainer[L], delta: L): AntiEntropyContainer[L] =
        c.applyDelta(Named(c.replicaID, Dotted(delta, Dots.empty)))
      override def query(c: AntiEntropyContainer[L]): L = c.state.data
      override def mutateContext(
          container: AntiEntropyContainer[L],
          withContext: Dotted[L]
      ): AntiEntropyContainer[L] = container.applyDelta(Named(container.replicaID, withContext))
      override def context(c: AntiEntropyContainer[L]): Dots = c.state.context
    }

  /** Creates a new PNCounter instance
    *
    * @param antiEntropy AntiEntropy instance used for exchanging deltas with other replicas
    */
  def apply[State](antiEntropy: AntiEntropy[State]): AntiEntropyContainer[State] =
    new AntiEntropyContainer(antiEntropy)
}
