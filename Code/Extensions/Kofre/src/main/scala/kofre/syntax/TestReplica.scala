package kofre.syntax

import kofre.base.{Bottom, Id, Lattice}

import scala.annotation.targetName

class TestReplica[A](val replicaId: Id, var anon: A) {
  def apply(delta: A)(using Lattice[A]): TestReplica[A] =
    anon = anon merge delta
    this
}

object TestReplica {

  @targetName("fromString")
  def apply[L](replicaId: String, anon: L): TestReplica[L] = apply(Id.predefined(replicaId), anon)
  def apply[L](replicaID: Id, anon: L): TestReplica[L]     = new TestReplica(replicaID, anon)
  def unapply[L](wnc: TestReplica[L]): Some[(Id, L)]       = Some((wnc.replicaId, wnc.anon))

  given permissions[L](using Lattice[L]): PermMutate[TestReplica[L], L]
  with {
    override def mutate(c: TestReplica[L], delta: L): TestReplica[L] = c.apply(delta)
    override def query(c: TestReplica[L]): L                         = c.anon
  }
}
