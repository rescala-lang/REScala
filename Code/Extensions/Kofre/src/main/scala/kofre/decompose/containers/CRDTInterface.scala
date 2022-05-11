package kofre.decompose.containers

import kofre.base.Defs.Id
import kofre.base.{DecomposeLattice, Defs}
import kofre.causality.CausalContext
import kofre.contextual.{ContextDecompose, ContextLattice, WithContext}
import kofre.syntax.WithNamedContext
import kofre.decompose.Delta
import kofre.syntax.{PermCausal, PermCausalMutate, PermIdMutate, PermQuery}

trait CRDTInterface[State, Wrapper] {

  val state: WithContext[State]

  val replicaID: Defs.Id

  def applyDelta(delta: WithNamedContext[State])(implicit u: DecomposeLattice[WithContext[State]]): Wrapper
}

object CRDTInterface {
  def allPermissions[L: ContextDecompose, B <: CRDTInterface[L, B]]: PermIdMutate[B, L] with PermCausalMutate[B, L] =
    new PermIdMutate[B, L] with PermCausalMutate[B, L] {
      override def replicaId(c: B): Id       = c.replicaID
      override def mutate(c: B, delta: L): B = c.applyDelta(Delta(c.replicaID, CausalContext.empty, delta))
      override def query(c: B): L            = c.state.store
      override def mutateContext(
          container: B,
          withContext: WithContext[L]
      ): B = container.applyDelta(Delta(container.replicaID, withContext.context, withContext.store))
      override def context(c: B): CausalContext = c.state.context
    }

  /** workaround to make existing syntax compile with different context decomposition */
  given focussedPermission[C, L](using outer: PermQuery[C, WithContext[L]]): PermQuery[C, L] = outer.focus(_.store)
}
