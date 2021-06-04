package rescala.extra.lattices

package object delta {
  type DeltaMutator[A]  = (String, A) => A
  type DeltaQuery[A, B] = A => B
}
