
package kofre.encrdt.lattices
import kofre.Lattice
object PairLattice {
  def pairLattice[A: Lattice, B: Lattice]: Lattice[(A, B)] =
    (left, right) => (
                       Lattice[A].merge(left._1, right._1),
                       Lattice[B].merge(left._2, right._2)
    )
}
