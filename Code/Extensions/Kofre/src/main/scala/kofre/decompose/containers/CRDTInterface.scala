package kofre.decompose.containers

import kofre.Defs
import kofre.Defs.Id
import kofre.decompose.{Delta, UIJDLattice}
import kofre.syntax.AllPermissionsCtx

trait CRDTInterface[State, Wrapper] {
  val state: State

  val replicaID: Defs.Id

  def applyDelta(delta: Delta[State])(implicit u: UIJDLattice[State]): Wrapper
}

object CRDTInterface {
  def crdtInterfaceContextPermissions[L: UIJDLattice, B <: CRDTInterface[L, B]]: AllPermissionsCtx[B, L] =
    new AllPermissionsCtx[B, L] {
      override def replicaId(c: B): Id       = c.replicaID
      override def mutate(c: B, delta: L): B = c.applyDelta(Delta(c.replicaID, delta))
      override def query(c: B): L            = c.state
    }
}
