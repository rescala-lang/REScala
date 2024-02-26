package rdts.syntax

import rdts.base.{Uid, Lattice}

import scala.annotation.targetName

class TestReplica[A](val replicaId: Uid, var anon: A) {
  def apply(delta: A)(using Lattice[A]): TestReplica[A] =
    anon = anon merge delta
    this
}

object TestReplica {

  @targetName("fromString")
  def apply[L](replicaId: String, anon: L): TestReplica[L] = apply(Uid.predefined(replicaId), anon)
  def apply[L](replicaID: Uid, anon: L): TestReplica[L]    = new TestReplica(replicaID, anon)
  def unapply[L](wnc: TestReplica[L]): Some[(Uid, L)]      = Some((wnc.replicaId, wnc.anon))

  given permissions[L](using Lattice[L]): PermMutate[TestReplica[L], L]
  with {
    override def mutate(c: TestReplica[L], delta: L): TestReplica[L] = c.apply(delta)
    override def query(c: TestReplica[L]): L                         = c.anon
  }
}
