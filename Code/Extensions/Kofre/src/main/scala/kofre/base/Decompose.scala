package kofre.base

/** This is kinda the reverse of a lattice. */
trait Decompose[A] {

  /** Decompose a state into smaller parts.
    * Note that the goal here is small individual storage size at reasonable computational cost.
    * Minimalism of returned results is not guaranteed.
    * It is also not guaranteed that the result does not overlap.
    */
  def decompose(a: A): Iterable[A]

  extension (a: A) def decomposed: Iterable[A] = decompose(a)

}
