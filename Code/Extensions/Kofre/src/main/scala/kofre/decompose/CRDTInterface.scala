package kofre.decompose

import kofre.syntax.{DeltaMutator, DeltaQuery}
import kofre.Defs

trait CRDTInterface[State, Wrapper] {
  val state: State

  val replicaID: Defs.Id

  protected def query[A](q: DeltaQuery[State, A]): A = q(state)

  def mutate(m: DeltaMutator[State])(implicit u: UIJDLattice[State]): Wrapper =
    applyDelta(Delta(replicaID, m(replicaID, state)))

  def applyDelta(delta: Delta[State])(implicit u: UIJDLattice[State]): Wrapper
}
