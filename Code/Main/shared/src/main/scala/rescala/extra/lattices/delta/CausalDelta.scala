package rescala.extra.lattices.delta

case class CausalDelta[D: DotStore, C: CContext](replicaID: String, deltaState: D, deltaCC: C)

case class SetDelta[D: DotStore](state: D, dots: Set[Dot])
