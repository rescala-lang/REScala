package kofre.decompose

import kofre.base.Defs
import kofre.causality.CausalContext

/** A Delta combines a delta state from the same state space as the CRDT state with the id of the replica propagating it.
  *
  * @tparam A Type of the delta state
  */
case class Delta[A](replicaID: Defs.Id, context: CausalContext, deltaState: A)
object Delta {
  def apply[A](replicaID: Defs.Id, delta: A): Delta[A] = Delta(replicaID, CausalContext.empty, delta)
}
