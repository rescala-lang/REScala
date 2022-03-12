package test.kofre

import org.scalatest.freespec.AnyFreeSpec
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks
import kofre.decompose.UIJDLattice
import kofre.Defs
import kofre.Lattice
import kofre.Lattice.Operators
import kofre.decompose.interfaces.AWSetInterface
import kofre.decompose.interfaces.AWSetInterface.State as AWSet

case class Replica[A](replicaid: Defs.Id, state: A)

trait Containement[C, L] {
  def query(c: C): L
  def replicaId(c: C): Defs.Id
  def mutate(c: C, delta: L): C
}

trait OpsSyntaxHelper[C, L] {
  type Mutation = Containement[C, L] ?=> C
  final def current(using c: Containement[C, L])(using a: C): L           = c.query(a)
  final def replicaId(using c: Containement[C, L])(using a: C): Defs.Id   = c.replicaId(a)
  final given mutate(using a: C, c: Containement[C, L]): Conversion[L, C] = c.mutate(a, _)
}

implicit class AWSetSyntax[E, C](container: C) extends OpsSyntaxHelper[C, AWSet[E]] {
  given C = container
  def add(elem: E): Mutation =
    AWSetInterface.add(elem).apply(replicaId, current)
}

given [A: Lattice]: Containement[Replica[A], A] with
  def query(c: Replica[A]): A                     = c.state
  def replicaId(c: Replica[A]): Defs.Id           = c.replicaid
  def mutate(c: Replica[A], delta: A): Replica[A] = c.copy(state = c.state merge delta)

given [L: Lattice]: Containement[L, L] with
  def query(c: L): L            = c
  def replicaId(c: L): Defs.Id  = "uhm ..."
  def mutate(c: L, delta: L): L = c merge delta

class SyntaxTest extends AnyFreeSpec {
  "huh" in {
    val awset = UIJDLattice[AWSet[String]].bottom

    val rep = Replica("tmp", awset)

    println(rep.add("yay"))
    println(awset.add("meh"))
  }
}
