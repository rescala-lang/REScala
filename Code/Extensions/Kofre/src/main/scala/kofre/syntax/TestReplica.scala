package kofre.syntax

import kofre.base.{Bottom, DecomposeLattice, Defs, Lattice}
import kofre.base.Defs.Id

class TestReplica[A](val replicaId: Id, var anon: A) {
  def apply(delta: A)(using Lattice[A]): TestReplica[A] =
    anon = anon merge delta
    this
}

object TestReplica {

  def apply[L](replicaID: Defs.Id, anon: L): TestReplica[L] = new TestReplica(replicaID, anon)
  def unapply[L](wnc: TestReplica[L]): Some[(Defs.Id, L)]   = Some((wnc.replicaId, wnc.anon))

  given permissions[L](using DecomposeLattice[L]): PermIdMutate[TestReplica[L], L]
    with {
    override def replicaId(c: TestReplica[L]): Id = c.replicaId
    override def mutate(c: TestReplica[L], delta: L): TestReplica[L] = c.apply(delta)
    override def query(c: TestReplica[L]): L = c.anon
  }

  given syntaxPassthroughTrans[K, L](using ArdtOpsContains[K, L]): ArdtOpsContains[TestReplica[K], L] = new {}
  given syntaxPassthrough[L]: ArdtOpsContains[TestReplica[L], L]                                      = new {}
}
