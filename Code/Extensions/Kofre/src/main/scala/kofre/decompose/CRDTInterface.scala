package kofre.decompose

import kofre.Defs
import kofre.Defs.Id
import kofre.syntax.{AllPermissionsCtx, DeltaMutator}

trait CRDTInterface[State, Wrapper] {
  val state: State

  val replicaID: Defs.Id


  def mutate(m: DeltaMutator[State])(implicit u: UIJDLattice[State]): Wrapper =
    applyDelta(Delta(replicaID, m(replicaID, state)))

  def applyDelta(delta: Delta[State])(implicit u: UIJDLattice[State]): Wrapper
}

object CRDTInterface {
  def crdtInterfaceContextPermissions[L: UIJDLattice, B <: CRDTInterface[L, B]]: AllPermissionsCtx[B, L] =
    new AllPermissionsCtx[B, L] {
      override def replicaId(c: B): Id       = c.replicaID
      override def mutate(c: B, delta: L): B = c.mutate((_, _) => delta)
      override def query(c: B): L            = c.state
    }
}
