package rescala.core

import rescala.core.Node.InDep

trait ComputationStateAccess[S <: Struct] {
  private[rescala] def staticBefore/*aka regRead/depRead*/[P](reactive: ReadableReactive[P, S]): P
  private[rescala] def staticAfter[P](reactive: ReadableReactive[P, S]): P
  private[rescala] def dynamicBefore[P](reactive: ReadableReactive[P, S]): P
  private[rescala] def dynamicAfter[P](reactive: ReadableReactive[P, S]): P
}

trait ReevaluationStateAccess[S <: Struct] {
  type ReevOutResult
  private[rescala] def drop(node: InDep[S], removeOutgoing: Reactive[S]): Unit
  private[rescala] def discover(node: InDep[S], addOutgoing: Reactive[S]): Unit

  // technically, above methods could could each add or remove the first parameter from the second parameter's indeps.
  // As reevaluation however computes the final indeps result anyways, this method here optimizes performance through
  // enabling a single bulk replacement update instead of multiple incremental changes.
  private[rescala] def writeIndeps(node: Reactive[S], indepsAfter: Set[InDep[S]]): Unit

//  private[rescala] def reevOutChanged[P](node: WriteableReactive[P, S], change: Pulse.Change[P]): Set[Reactive[S]]
//  private[rescala] def reevOutUnchanged[P](node: WriteableReactive[P, S]): Set[Reactive[S]]
}
