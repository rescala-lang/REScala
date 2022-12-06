package kofre.syntax

import kofre.base.{Bottom, DecomposeLattice, Defs}
import kofre.base.Defs.Id
import kofre.time.Dots
import kofre.dotted.{Dotted, DottedLattice}

case class DottedName[L](replicaID: Defs.Id, anon: Dotted[L]) {
  def map[B](f: L => B): DottedName[B] = new DottedName(replicaID, anon.map(f))
}

object DottedName {

  def empty[A: Bottom](replicaId: Defs.Id) = new DottedName(replicaId, Dotted(Bottom.empty[A], Dots.empty))

  def apply[L](replicaID: Defs.Id, inner: Dotted[L]): DottedName[L] = new DottedName(replicaID, inner)
  def unapply[L](wnc: DottedName[L]): Some[(Defs.Id, Dotted[L])]    = Some((wnc.replicaID, wnc.anon))

  given permissions[L](using DecomposeLattice[Dotted[L]]): PermQuery[DottedName[L], L]
    with PermId[DottedName[L]] with PermCausal[DottedName[L]] with PermCausalMutate[DottedName[L], L]
    with {
    override def replicaId(c: DottedName[L]): Id = c.replicaID
    override def mutateContext(c: DottedName[L], delta: Dotted[L]): DottedName[L] =
      DottedName(c.replicaID, c.anon merge delta)
    override def query(c: DottedName[L]): L      = c.anon.store
    override def context(c: DottedName[L]): Dots = c.anon.context
  }

  given syntaxPassthroughTrans[K, L](using ArdtOpsContains[K, L]): ArdtOpsContains[DottedName[K], L] = new {}
  given syntaxPassthrough[L]: ArdtOpsContains[DottedName[L], L]                                      = new {}
}
