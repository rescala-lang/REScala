package kofre.decompose.containers

import kofre.base.Defs.Id
import kofre.base.{DecomposeLattice, Defs}
import kofre.decompose.Delta
import kofre.syntax.PermIdMutate

trait CRDTInterface[State, Wrapper] {
  val state: State

  val replicaID: Defs.Id

  def applyDelta(delta: Delta[State])(implicit u: DecomposeLattice[State]): Wrapper
}

object CRDTInterface {
  def crdtInterfaceContextPermissions[L: DecomposeLattice, B <: CRDTInterface[L, B]]: PermIdMutate[B, L] =
    new PermIdMutate[B, L] {
      override def replicaId(c: B): Id       = c.replicaID
      override def mutate(c: B, delta: L): B = c.applyDelta(Delta(c.replicaID, delta))
      override def query(c: B): L            = c.state
    }
}
