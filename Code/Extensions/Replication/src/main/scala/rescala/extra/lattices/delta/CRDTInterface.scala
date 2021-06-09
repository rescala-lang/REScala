package rescala.extra.lattices.delta

import rescala.extra.lattices.delta.CRDTInterface.{DeltaMutator, DeltaQuery}

trait CRDTInterface[State, Wrapper] {
  val state: State

  val replicaID: String

  protected def query[A](q: DeltaQuery[State, A]): A = q(state)

  protected def mutate(m: DeltaMutator[State])(implicit u: UIJDLattice[State]): Wrapper =
    applyDelta(Delta(replicaID, m(replicaID, state)))

  def applyDelta(delta: Delta[State])(implicit u: UIJDLattice[State]): Wrapper
}

object CRDTInterface {
  type DeltaMutator[A]  = (String, A) => A
  type DeltaQuery[A, B] = A => B
}
