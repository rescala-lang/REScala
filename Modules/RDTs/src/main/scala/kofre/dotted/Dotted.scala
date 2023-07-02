package kofre.dotted

import kofre.base.{Bottom, Lattice}
import kofre.syntax.{PermCausalMutate, PermMutate, ReplicaId}
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
  def advanced(r: ReplicaId): Dotted[A] = Dotted(data, context.advanced(r.uid))
}

type DottedLattice[T] = Lattice[Dotted[T]]
object DottedLattice {
  export Lattice.{apply as _, *}

  def apply[A](using ev: Lattice[Dotted[A]]): Lattice[Dotted[A]] = ev
}

object Dotted {

  def empty[A: Bottom]: Dotted[A] = Dotted(Bottom.empty[A], Dots.empty)
  def apply[A](a: A): Dotted[A]   = Dotted(a, Dots.empty)

  given lattice[A: HasDots: Bottom: Lattice]: Lattice[Dotted[A]] with {
    def merge(left: Dotted[A], right: Dotted[A]): Dotted[A] =
      val l = left.data.removeDots(right.deletions).getOrElse(Bottom.empty)
      val r = right.data.removeDots(left.deletions).getOrElse(Bottom.empty)
      Dotted(l merge r, left.context union right.context)
  }

  def liftLattice[A: Lattice]: Lattice[Dotted[A]] = new {
    def merge(left: Dotted[A], right: Dotted[A]): Dotted[A] =
      Dotted(left.data merge right.data, left.context union right.context)
  }

  given syntaxPermissions[L](using Lattice[Dotted[L]]): PermCausalMutate[Dotted[L], L] with PermMutate[Dotted[L], L]
    with {
    override def mutateContext(c: Dotted[L], delta: Dotted[L]): Dotted[L] = c merge delta
    override def mutate(c: Dotted[L], delta: L): Dotted[L]                = c merge Dotted(delta)
    override def query(c: Dotted[L]): L                                   = c.data
    override def context(c: Dotted[L]): Dots                              = c.context
  }

}
