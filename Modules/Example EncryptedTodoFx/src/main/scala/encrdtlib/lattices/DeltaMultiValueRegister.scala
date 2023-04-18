package encrdtlib.lattices

import kofre.base.{Uid, Lattice}
import kofre.dotted.{DotFun, Dotted, HasDots}
import kofre.time.{Dot, Dots}

object DeltaMultiValueRegister {
  type DeltaMultiValueRegisterLattice[V] = Dotted[DotFun[V]]

  def deltaWrite[V](
      value: V,
      replicaId: Uid,
      register: DeltaMultiValueRegisterLattice[V]
  ): DeltaMultiValueRegisterLattice[V] = {

    val dot = register.context.nextDot(replicaId)
    Dotted(
      DotFun(Map(dot -> value)),
      Dots.from(register.data.repr.keySet + dot)
    )
  }

  def deltaClear[V: Lattice](register: DeltaMultiValueRegisterLattice[V]): DeltaMultiValueRegisterLattice[V] =
    Dotted(
      DotFun.empty,
      Dots.from(register.data.repr.keySet)
    )

  def read[V](register: DeltaMultiValueRegisterLattice[V]): Set[V] = {
    register.data.repr.values.toSet
  }
}
