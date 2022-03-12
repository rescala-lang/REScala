package kofre.syntax

import kofre.Defs

trait DeltaMutator[T] {
  def apply(replicaId: Defs.Id, base: T): T
}

type DeltaQuery[A, B] = A => B
