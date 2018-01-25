package rescala.sharedimpl

import rescala.core.{DynamicTicket, ReSource, Reactive, Struct}

trait ReevaluationStateAccess[S <: Struct] {

  final def commitDependencyDiff(node: Reactive[S], dt: DynamicTicket[S]): Unit = {
    if(dt.indepsChanged) {
      dt.indepsRemoved.foreach(drop(_, node))
      dt.indepsAdded.foreach(discover(_, node))
      writeIndeps(node, dt.indepsAfter)
    }
  }

  private[rescala] def drop(node: ReSource[S], removeOutgoing: Reactive[S]): Unit
  private[rescala] def discover(node: ReSource[S], addOutgoing: Reactive[S]): Unit

  // technically, above methods could could each add or remove the first parameter from the second parameter's indeps.
  // As reevaluation however computes the final indeps result anyways, this method here optimizes performance through
  // enabling a single bulk replacement update instead of multiple incremental changes.
  private[rescala] def writeIndeps(node: Reactive[S], indepsAfter: Set[ReSource[S]]): Unit
}
