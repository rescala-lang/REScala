package encrdtlib.lattices

import kofre.base.{Id, Lattice}
import kofre.dotted.{DotFun, Dotted, HasDots}
import kofre.time.{Dot, Dots}

object DeltaMultiValueRegister {
  type DeltaMultiValueRegisterLattice[V] = Dotted[DotFun[V]]

  def deltaWrite[V](
      value: V,
      replicaId: Id,
      register: DeltaMultiValueRegisterLattice[V]
  ): DeltaMultiValueRegisterLattice[V] = {

    val dot = register.context.clockOf(replicaId).get.advance
    Dotted(
      DotFun(Map(dot -> value)),
      Dots.from(register.store.keySet + dot)
    )
  }

  def deltaClear[V: Lattice](register: DeltaMultiValueRegisterLattice[V]): DeltaMultiValueRegisterLattice[V] =
    Dotted(
      DotFun.empty,
      Dots.from(register.store.keySet)
    )

  def read[V](register: DeltaMultiValueRegisterLattice[V]): Set[V] = {
    register.store.values.toSet
  }
}
