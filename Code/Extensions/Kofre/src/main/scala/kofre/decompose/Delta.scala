package kofre.decompose

import kofre.base.Defs
import kofre.time.Dots
import kofre.syntax.WithNamedContext
import kofre.contextual.Dotted

/** A Delta combines a delta state from the same state space as the CRDT state with the id of the replica propagating it.
  *
  * @tparam A Type of the delta state
  */
object Delta {
  def apply[A](replicaID: Defs.Id, delta: A): WithNamedContext[A] = WithNamedContext(replicaID, Dotted(delta, Dots.empty))
  def apply[A](replicaID: Defs.Id, context: Dots, delta: A): WithNamedContext[A] = WithNamedContext(replicaID, Dotted(delta, context))
  def unapply[A](wnc: WithNamedContext[A]): Option[(Defs.Id, Dots, A)] = Some((wnc.replicaID, wnc.anon.context, wnc.anon.store))
}
