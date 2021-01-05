package rescala.extra.lattices.delta

case class Delta[A: UIJDLattice](replicaID: String, deltaState: A)
