package kofre.decompose

/** A Delta combines a delta state from the same state space as the CRDT state with the id of the replica propagating it.
  *
  * @tparam A Type of the delta state
  */
case class Delta[A](replicaID: String, deltaState: A)
