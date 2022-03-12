package test.kofre

import org.scalatest.freespec.AnyFreeSpec
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks
import kofre.decompose.UIJDLattice
import kofre.Defs
import kofre.Lattice
import kofre.Lattice.Operators
import kofre.decompose.interfaces.AWSetInterface
import kofre.decompose.interfaces.AWSetInterface.State as AWSet

import kofre.syntax.*

case class Replica[A](replicaid: Defs.Id, state: A)


implicit class AWSetSyntax[E, C](container: C) extends OpsSyntaxHelper[C, AWSet[E]](container) {
  def add(elem: E): MutationID =
    AWSetInterface.add(elem).apply(replicaID, current)
}

given [A: Lattice]: AllPermissionsCtx[Replica[A], A] with
  def query(c: Replica[A]): A                     = c.state
  def replicaId(c: Replica[A]): Defs.Id           = c.replicaid
  def mutate(c: Replica[A], delta: A): Replica[A] = c.copy(state = c.state merge delta)

given [L: Lattice]: MutateCtx[L, L] with
  def query(c: L): L            = c
  def mutate(c: L, delta: L): L = c merge delta

class SyntaxTest extends AnyFreeSpec {
  "huh" in {
    val awset = UIJDLattice[AWSet[String]].bottom

    val rep = Replica("tmp", awset)

    println(rep.add("yay"))
  }
}
