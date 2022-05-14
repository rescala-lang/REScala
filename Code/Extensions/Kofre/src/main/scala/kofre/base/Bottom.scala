package kofre.base

import kofre.dotted.{ContextDecompose, Dotted, HasDots}

/** Bottom.empty is the identity of Lattice.merge */
trait Bottom[A] {
  def empty: A
}
object Bottom {
  def empty[A](using bottom: Bottom[A]): A = bottom.empty
  def apply[A](using bottom: Bottom[A]): Bottom[A] = bottom

  // Forwarders when bottom is requested but on of the others could work
  given decomposeBottom[A](using dl: DecomposeLattice[A]): Bottom[A] = dl
  given withoutContextBottom[A](using acc: ContextDecompose[A]): Bottom[A] with
    def empty: A = acc.empty.store
}
