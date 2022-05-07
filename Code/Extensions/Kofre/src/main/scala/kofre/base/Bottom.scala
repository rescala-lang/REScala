package kofre.base

import kofre.contextual.WithContextDecompose
import kofre.decompose.{Decompose, DecomposeLattice}

/** Bottom.empty is the identity of Lattice.merge */
trait Bottom[A] {
  def empty: A
}
object Bottom {
  def empty[A](using bottom: Bottom[A]): A = bottom.empty

  given decomposeBottom[A](using dl: DecomposeLattice[A]): Bottom[A] = dl
}
