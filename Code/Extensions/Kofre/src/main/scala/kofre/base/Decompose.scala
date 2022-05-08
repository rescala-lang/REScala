package kofre.base

trait Decompose[A] {

  /** Decomposes a lattice state into its unique irredundant join decomposition of join-irreducible states */
  def decompose(a: A): Iterable[A]
}
