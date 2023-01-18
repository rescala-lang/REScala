package kofre.syntax

import kofre.base.{Bottom, Id, Lattice}
import kofre.dotted.{Dotted, DottedLattice}
import kofre.time.Dots

case class Named[L](replicaId: Id, anon: L) {
  def map[B](f: L => B): Named[B] = new Named(replicaId, f(anon))
}

object Named {

  given permissionsDN[L](using Lattice[Dotted[L]]): PermId[Named[Dotted[L]]]
    with PermCausalMutate[Named[Dotted[L]], L]
    with {
    override def replicaId(c: Named[Dotted[L]]): Id = c.replicaId
    override def mutateContext(c: Named[Dotted[L]], delta: Dotted[L]): Named[Dotted[L]] =
      Named(c.replicaId, c.anon merge delta)
    override def query(c: Named[Dotted[L]]): L      = c.anon.store
    override def context(c: Named[Dotted[L]]): Dots = c.anon.context
  }

  def empty[A: Bottom](replicaId: Id) = new Named(replicaId, Bottom.empty[A])

  given permissions[L](using Lattice[L]): PermIdMutate[Named[L], L] with {
    override def replicaId(c: Named[L]): Id              = c.replicaId
    override def query(c: Named[L]): L                   = c.anon
    override def mutate(c: Named[L], delta: L): Named[L] = Named(c.replicaId, c.anon merge delta)
  }

}
