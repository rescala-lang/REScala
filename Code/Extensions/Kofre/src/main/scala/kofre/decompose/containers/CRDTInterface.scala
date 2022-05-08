package kofre.decompose.containers

import kofre.base.Defs.Id
import kofre.base.{DecomposeLattice, Defs}
import kofre.causality.CausalContext
import kofre.contextual.WithContext
import kofre.decompose.Delta
import kofre.syntax.{PermCausal, PermCausalMutate, PermIdMutate, PermQuery}

trait CRDTInterface[State, Wrapper] {
  val state: State

  val replicaID: Defs.Id

  def applyDelta(delta: Delta[State])(implicit u: DecomposeLattice[State]): Wrapper
}

object CRDTInterface {
  def crdtInterfaceContextPermissions[L: DecomposeLattice, B <: CRDTInterface[L, B]]: PermIdMutate[B, L] =
    new PermIdMutate[B, L] {
      override def replicaId(c: B): Id       = c.replicaID
      override def mutate(c: B, delta: L): B = c.applyDelta(Delta(c.replicaID, CausalContext.empty, delta))
      override def query(c: B): L            = c.state
    }

  def contextPermissions[L, B <: CRDTInterface[WithContext[L], B]](using
      DecomposeLattice[WithContext[L]]
  ): PermCausalMutate[B, L] with PermCausal[B] =
    new PermCausalMutate[B, L] with PermCausal[B] {
      override def mutateContext(
          container: B,
          withContext: WithContext[L]
      ): B =
        container.applyDelta(Delta(container.replicaID, CausalContext.empty, withContext))
      override def context(c: B): CausalContext = c.state.context
    }

  /** workaround to make existing syntax compile with different context decomposition */
  given focussedPermission[C, L](using outer: PermQuery[C, WithContext[L]]): PermQuery[C, L] = outer.focus(_.store)
}
