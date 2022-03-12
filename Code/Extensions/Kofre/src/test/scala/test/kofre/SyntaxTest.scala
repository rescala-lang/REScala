package test.kofre

import org.scalatest.freespec.AnyFreeSpec
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks
import kofre.decompose.interfaces.AWSetModule
import kofre.decompose.interfaces.AWSet
import kofre.decompose.UIJDLattice
import kofre.Defs
import kofre.Lattice
import kofre.Lattice.Operators


case class Replica[A](replicaid: Defs.Id, state: A)

trait Containement[C, T] {
  def query(c: C): T
  def replicaId(c: C): Defs.Id
  def mutate(c: C, delta: T): C
}

implicit class AWSetSyntax[E, C](container: C)(using c: Containement[C, AWSetModule.AWSet[E]]) {
  def add(elem: E): C =
    c.mutate(container, AWSet.add(elem).apply(c.replicaId(container), c.query(container)))
}

given [A: Lattice]: Containement[Replica[A], A] with
  def query(c: Replica[A]): A = c.state
  def replicaId(c: Replica[A]): Defs.Id = c.replicaid
  def mutate(c: Replica[A], delta: A): Replica[A] = c.copy(state = c.state marge delta)

class SyntaxTest extends AnyFreeSpec {
  "huh" in {
    val awset = AWSetModule.awSetLattice[String].bottom

    val rep = Replica("tmp", awset)

    rep.add("yay")
  }
}
