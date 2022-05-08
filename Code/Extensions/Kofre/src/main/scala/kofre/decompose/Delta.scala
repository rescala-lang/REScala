package kofre.decompose

import kofre.base.Defs
import kofre.causality.CausalContext
import kofre.syntax.WithNamedContext
import kofre.contextual.WithContext

/** A Delta combines a delta state from the same state space as the CRDT state with the id of the replica propagating it.
  *
  * @tparam A Type of the delta state
  */
object Delta {
  def apply[A](replicaID: Defs.Id, delta: A): WithNamedContext[A] = WithNamedContext(replicaID, WithContext(delta, CausalContext.empty))
  def apply[A](replicaID: Defs.Id, context: CausalContext, delta: A): WithNamedContext[A] = WithNamedContext(replicaID, WithContext(delta, context))
  def unapply[A](wnc: WithNamedContext[A]): Option[(Defs.Id, CausalContext, A)] = Some((wnc.replicaID, wnc.inner.context, wnc.inner.store))
}
