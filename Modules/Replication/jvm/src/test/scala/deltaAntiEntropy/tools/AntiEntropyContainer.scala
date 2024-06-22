package deltaAntiEntropy.tools

import rdts.base.Uid.asId
import rdts.base.{Lattice, Uid}
import rdts.dotted.{Dotted, DottedLattice}
import rdts.syntax.{LocalUid}
import rdts.time.Dots

import scala.annotation.targetName

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
  val replicaID: LocalUid = antiEntropy.replicaID.asId

  def state: Dotted[State] = antiEntropy.state

  override def toString: String =
    s"AntiEntropy($replicaID, $state)"

  inline def map(f: LocalUid ?=> State => State)(using Lattice[Dotted[State]]): AntiEntropyContainer[State] =
    applyDelta(Named(replicaID.uid, Dotted(f(using replicaID)(state.data))))

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

  extension [A](curr: AntiEntropyContainer[A])(using Lattice[Dotted[A]]) {
    inline def mod(f: Dots ?=> A => Dotted[A]): AntiEntropyContainer[A] = {
      curr.applyDelta(Named(curr.replicaID.uid, curr.state.mod(f(_))))
    }
  }

  extension [A](curr: AntiEntropyContainer[A]) {
    def data: A = curr.state.data
  }


  extension [A](curr: AntiEntropyContainer[A])(using Lattice[Dotted[A]]) {
    @targetName("modNoDelta") inline def modn(f: A => A): AntiEntropyContainer[A] = {
      val next = Dots.single(curr.state.context.nextDot(curr.replicaID.uid))
      curr.applyDelta(Named(curr.replicaID.uid, Dotted(f(curr.state.data), next)))
    }
  }

  /** Creates a new PNCounter instance
    *
    * @param antiEntropy AntiEntropy instance used for exchanging deltas with other replicas
    */
  def apply[State](antiEntropy: AntiEntropy[State]): AntiEntropyContainer[State] =
    new AntiEntropyContainer(antiEntropy)
}
