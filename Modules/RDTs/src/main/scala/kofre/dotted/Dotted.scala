package kofre.dotted

import kofre.base.{Bottom, Lattice}
import kofre.syntax.{PermCausalMutate, PermMutate}
import kofre.time.{Dot, Dots}

/** Associates a context of Dots with some data structure.
  * The most common use is to interpret the context as the set of:
  * • all dots that are present in data
  * • all dots directly subsumed by data
  *
  * Specifically, the `deletions` and `contained` methods reflect this interpretation.
  */
case class Dotted[A](data: A, context: Dots) {
  def map[B](f: A => B): Dotted[B]      = Dotted(f(data), context)
  def knows(dot: Dot): Boolean          = context.contains(dot)
  def deletions(using HasDots[A]): Dots = context diff contained
  def contained(using HasDots[A]): Dots = data.dots
}

object Dotted extends Dotted.LowPrio {

  def empty[A: Bottom]: Dotted[A] = Dotted(Bottom.empty[A], Dots.empty)
  def apply[A](a: A): Dotted[A]   = Dotted(a, Dots.empty)


  given dottedLattice[A: HasDots: Bottom: Lattice]: DottedLattice[A] with {
    def mergePartial(left: Dotted[A], right: Dotted[A]): A =
      val l = left.data.removeDots(right.deletions).getOrElse(Bottom.empty)
      val r = right.data.removeDots(left.deletions).getOrElse(Bottom.empty)
      l merge r
  }

  // causes DottedLattice instance to be found in some cases where we are only looking for a Lattice[Dotted[A]]
  export DottedLattice.given

  given syntaxPermissions[L](using DottedLattice[L]): PermCausalMutate[Dotted[L], L] with {
    override def mutateContext(c: Dotted[L], delta: Dotted[L]): Dotted[L] = c merge delta
    override def query(c: Dotted[L]): L                                   = c.data
    override def context(c: Dotted[L]): Dots                              = c.context
  }

  trait LowPrio {
    given identitySyntaxPermissions[L](using DottedLattice[L]): PermMutate[Dotted[L], L] with {
      override def mutate(c: Dotted[L], delta: L): Dotted[L] = c merge Dotted(delta)

      override def query(c: Dotted[L]): L = c.data
    }
  }

}
