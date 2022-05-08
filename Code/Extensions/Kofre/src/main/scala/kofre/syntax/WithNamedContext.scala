package kofre.syntax

import kofre.base.DecomposeLattice
import kofre.base.Defs.Id
import kofre.causality.CausalContext
import kofre.contextual.{WithContext, WithContextMerge}

class WithNamedContext[L](val replicaID: kofre.base.Defs.Id, val inner: WithContext[L]) {}

object WithNamedContext {

  given withContextPermission[L](using DecomposeLattice[WithContext[L]]): PermQuery[WithNamedContext[L], L]
    with PermId[WithNamedContext[L]] with PermCausal[WithNamedContext[L]] with PermCausalMutate[WithNamedContext[L], L]
    with {
    override def replicaId(c: WithNamedContext[L]): Id = c.replicaID
    override def mutateContext(c: WithNamedContext[L], delta: WithContext[L]): WithNamedContext[L] =
      WithNamedContext(c.replicaID, c.inner merged delta)
    override def query(c: WithNamedContext[L]): L = c.inner.store
    override def context(c: WithNamedContext[L]): CausalContext = c.inner.context
  }
}
