package rescala.extra.lattices.delta

case class CausalDelta[D: DotStore, C: CContext](deltaState: D, deltaCC: C)

// use this instead of CausalDelta in DotStore Interfaces so that they do not need to know how to build a causal context
case class SetDelta[D: DotStore](state: D, dots: Set[Dot])
