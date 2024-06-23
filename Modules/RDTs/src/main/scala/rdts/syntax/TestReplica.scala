package rdts.syntax

import rdts.base.{Lattice, LocalUid, Uid}

class TestReplica[A](val replicaId: LocalUid, var anon: A) {
  def apply(delta: A)(using Lattice[A]): TestReplica[A] =
    anon = anon merge delta
    this

  def mod(f: A => LocalUid ?=> A)(using Lattice[A]) = {
    apply(f(anon)(using replicaId))
  }
}

object TestReplica {

//  @targetName("fromString")
//  def apply[L](replicaId: String, anon: L): TestReplica[L] = apply(Uid.predefined(replicaId), anon)
  def apply[L](replicaID: Uid, anon: L): TestReplica[L] = new TestReplica(replicaID, anon)
  def unapply[L](wnc: TestReplica[L]): Some[(Uid, L)]   = Some((wnc.replicaId.uid, wnc.anon))

}
