package kofre.decompose.containers

import kofre.base.Defs.Id
import kofre.base.{DecomposeLattice, Defs}
import kofre.time.Dots
import kofre.contextual.{ContextDecompose, ContextLattice, Dotted}
import kofre.syntax.DottedName
import kofre.decompose.Delta
import kofre.syntax.{PermCausal, PermCausalMutate, PermIdMutate, PermQuery}
import kofre.base.Lattice

trait CRDTInterface[State, Wrapper] {

  def state: Dotted[State]

  val replicaID: Defs.Id

  def applyDelta(delta: DottedName[State])(implicit u: DecomposeLattice[Dotted[State]]): Wrapper
}

object CRDTInterface {
  def dottedPermissions[L: ContextDecompose, B <: CRDTInterface[L, B]]: PermIdMutate[B, L] with PermCausalMutate[B, L] =
    new PermIdMutate[B, L] with PermCausalMutate[B, L] {
      override def replicaId(c: B): Id       = c.replicaID
      override def mutate(c: B, delta: L): B = c.applyDelta(Delta(c.replicaID, Dots.empty, delta))
      override def query(c: B): L            = c.state.store
      override def mutateContext(
          container: B,
          withContext: Dotted[L]
      ): B = container.applyDelta(DottedName(container.replicaID, withContext))
      override def context(c: B): Dots = c.state.context
    }

  def fullPermissions[L: DecomposeLattice, B <: CRDTInterface[L, B]]: PermIdMutate[B, L] =
    new PermIdMutate[B, L] {
      override def replicaId(c: B): Id       = c.replicaID
      override def mutate(c: B, delta: L): B = c.applyDelta(DottedName(c.replicaID, Dotted(delta)))
      override def query(c: B): L            = c.state.store
    }

  /** workaround to make existing syntax compile with different context decomposition */
  //given focussedPermission[C, L](using outer: PermQuery[C, Dotted[L]]): PermQuery[C, L] = outer.focus(_.store)
}
