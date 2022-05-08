package kofre.syntax

import kofre.base.{Bottom, DecomposeLattice, Defs}
import kofre.base.Defs.Id
import kofre.causality.CausalContext
import kofre.contextual.{WithContext, WithContextMerge}

class WithNamedContext[L](val replicaID: Defs.Id, val inner: WithContext[L])

object WithNamedContext {

  def empty[A: Bottom](replicaId: Defs.Id) = new WithNamedContext(replicaId, WithContext(Bottom.empty[A], CausalContext.empty))

  def apply[L](replicaID: Defs.Id, inner: WithContext[L]): WithNamedContext[L] = new WithNamedContext(replicaID, inner)

  given permissions[L](using DecomposeLattice[WithContext[L]]): PermQuery[WithNamedContext[L], L]
    with PermId[WithNamedContext[L]] with PermCausal[WithNamedContext[L]] with PermCausalMutate[WithNamedContext[L], L]
    with {
    override def replicaId(c: WithNamedContext[L]): Id = c.replicaID
    override def mutateContext(c: WithNamedContext[L], delta: WithContext[L]): WithNamedContext[L] =
      WithNamedContext(c.replicaID, c.inner merged delta)
    override def query(c: WithNamedContext[L]): L               = c.inner.store
    override def context(c: WithNamedContext[L]): CausalContext = c.inner.context
  }
}
