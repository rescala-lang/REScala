package kofre.syntax

import kofre.base.{Bottom, DecomposeLattice, Defs}
import kofre.base.Defs.Id
import kofre.time.Dots
import kofre.contextual.{Dotted, ContextLattice}

class WithNamedContext[L](val replicaID: Defs.Id, val anon: Dotted[L]) {
  def map[B](f: L => B): WithNamedContext[B] = new WithNamedContext(replicaID, anon.map(f))
}

object WithNamedContext {

  def empty[A: Bottom](replicaId: Defs.Id) = new WithNamedContext(replicaId, Dotted(Bottom.empty[A], Dots.empty))

  def apply[L](replicaID: Defs.Id, inner: Dotted[L]): WithNamedContext[L] = new WithNamedContext(replicaID, inner)
  def unapply[L](wnc: WithNamedContext[L]): Some[(Defs.Id, Dotted[L])] = Some((wnc.replicaID, wnc.anon))

  given permissions[L](using DecomposeLattice[Dotted[L]]): PermQuery[WithNamedContext[L], L]
                                                           with PermId[WithNamedContext[L]] with PermCausal[WithNamedContext[L]] with PermCausalMutate[WithNamedContext[L], L]
    with {
    override def replicaId(c: WithNamedContext[L]): Id = c.replicaID
    override def mutateContext(c: WithNamedContext[L], delta: Dotted[L]): WithNamedContext[L] =
      val res = c.anon merged delta
      WithNamedContext(c.replicaID, c.anon merged delta)
    override def query(c: WithNamedContext[L]): L               = c.anon.store
    override def context(c: WithNamedContext[L]): Dots = c.anon.context
  }

  given syntaxPassthrough[L]: ArdtOpsContains[WithNamedContext[L], L] = new ArdtOpsContains[WithNamedContext[L], L] {}
}
