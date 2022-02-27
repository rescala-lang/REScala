package kofre.decompose

import kofre.decompose.CRDTInterface.{DeltaMutator, DeltaQuery}
import kofre.Defs

trait CRDTInterface[State, Wrapper] {
  val state: State

  val replicaID: Defs.Id

  protected def query[A](q: DeltaQuery[State, A]): A = q(state)

  def mutate(m: DeltaMutator[State])(implicit u: UIJDLattice[State]): Wrapper =
    applyDelta(Delta(replicaID, m(replicaID, state)))

  def applyDelta(delta: Delta[State])(implicit u: UIJDLattice[State]): Wrapper
}

object CRDTInterface {
  type DeltaMutator[A]  = (Defs.Id, A) => A
  type DeltaQuery[A, B] = A => B
}
