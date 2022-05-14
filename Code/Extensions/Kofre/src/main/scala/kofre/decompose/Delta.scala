package kofre.decompose

import kofre.base.Defs
import kofre.dotted.Dotted
import kofre.time.Dots
import kofre.syntax.DottedName

/** A Delta combines a delta state from the same state space as the CRDT state with the id of the replica propagating it.
  *
  * @tparam A Type of the delta state
  */
object Delta {
  def apply[A](replicaID: Defs.Id, delta: A): DottedName[A] = DottedName(replicaID, Dotted(delta, Dots.empty))
  def apply[A](replicaID: Defs.Id, context: Dots, delta: A): DottedName[A] = DottedName(replicaID, Dotted(delta, context))
  def unapply[A](wnc: DottedName[A]): Option[(Defs.Id, Dots, A)] = Some((wnc.replicaID, wnc.anon.context, wnc.anon.store))
}
