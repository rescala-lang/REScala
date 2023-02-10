package kofre.syntax

import kofre.base.{Bottom, Id, Lattice}
import kofre.dotted.{Dotted, DottedLattice}
import kofre.time.Dots

case class Named[L](replicaId: Id, anon: L) {
  def map[B](f: L => B): Named[B] = new Named(replicaId, f(anon))
}
