package kofre.syntax

import kofre.base.{Bottom, DecomposeLattice, Id, Lattice}
import kofre.time.Dots
import kofre.dotted.{Dotted, DottedLattice}

case class DottedName[L](replicaID: Id, anon: Dotted[L]) {
  def map[B](f: L => B): DottedName[B] = new DottedName(replicaID, anon.map(f))
}

object DottedName {

  def empty[A: Bottom](replicaId: Id) = new DottedName(replicaId, Dotted(Bottom.empty[A], Dots.empty))

  def apply[L](replicaID: Id, inner: Dotted[L]): DottedName[L] = new DottedName(replicaID, inner)
  def unapply[L](wnc: DottedName[L]): Some[(Id, Dotted[L])]    = Some((wnc.replicaID, wnc.anon))

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

case class AnyNamed[L](replicaID: Id, anon: L) {
  def map[B](f: L => B): AnyNamed[B] = new AnyNamed(replicaID, f(anon))
}

object AnyNamed {

  def empty[A: Bottom](replicaId: Id) = new AnyNamed(replicaId, Bottom.empty[A])

  given permissions[L](using Lattice[L]): PermQuery[AnyNamed[L], L]
    with PermId[AnyNamed[L]]
    with PermMutate[AnyNamed[L], L]
    with PermIdMutate[AnyNamed[L], L]
    with {
    override def replicaId(c: AnyNamed[L]): Id                 = c.replicaID
    override def query(c: AnyNamed[L]): L                      = c.anon
    override def mutate(c: AnyNamed[L], delta: L): AnyNamed[L] = AnyNamed(c.replicaID, c.anon merge delta)
  }

  given syntaxPassthroughTrans[K, L](using ArdtOpsContains[K, L]): ArdtOpsContains[AnyNamed[K], L] = new {}
  given syntaxPassthrough[L]: ArdtOpsContains[AnyNamed[L], L]                                      = new {}
}
