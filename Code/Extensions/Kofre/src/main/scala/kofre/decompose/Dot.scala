package rescala.extra.lattices.delta

/** Dots are globally unique counters that are used to track causality in causal CRDTs. To guarantee global uniqueness,
  * dots combine a globally unique replicaID with a locally unique counter.
  */
case class Dot(replicaID: String, counter: Int) {
  def next: Dot = Dot(replicaID, counter + 1)
}
