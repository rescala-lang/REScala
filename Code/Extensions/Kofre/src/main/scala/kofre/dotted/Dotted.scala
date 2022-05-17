package kofre.dotted

import kofre.base.Lattice.Operators
import kofre.base.{Bottom, DecomposeLattice, Lattice}
import kofre.dotted.{DotFun, DotSet}
import kofre.syntax.{ArdtOpsContains, DottedName, PermCausal, PermCausalMutate, PermQuery}
import kofre.time.{Dot, Dots}

import scala.util.NotGiven

case class Dotted[A](store: A, context: Dots) {
  def map[B](f: A => B): Dotted[B]                 = Dotted(f(store), context)
  def named(id: kofre.base.Defs.Id): DottedName[A] = DottedName(id, this)
}

/** Implicit aliases in companion object for search path */
object Dotted {

  def empty[A: Bottom]: Dotted[A] = Dotted(Bottom.empty[A], Dots.empty)
  def apply[A](a: A): Dotted[A]   = Dotted(a, Dots.empty)

  given CausalWithDotSetLattice: DecomposeLattice[Dotted[Set[Dot]]] =
    DotSet.contextDecompose.contextbimap[Set[Dot]](
      _.map(_.repr.toSet),
      _.map(s => DotSet(Dots.fromSet(s)))
    )

  given latticeLift[L: DecomposeLattice: Bottom]: DecomposeLattice[Dotted[L]] = DecomposeLattice.derived

  given syntaxPermissions[L](using DottedLattice[L]): PermCausalMutate[Dotted[L], L]
    with {
    override def mutateContext(c: Dotted[L], delta: Dotted[L]): Dotted[L] = c merged delta
    override def query(c: Dotted[L]): L                                   = c.store
    override def context(c: Dotted[L]): Dots                              = c.context
  }

  given syntaxPassthrough[L]: ArdtOpsContains[Dotted[L], L] = new ArdtOpsContains[Dotted[L], L] {}
}
