package encrdtlib.lattices

import kofre.base.{Uid, Lattice}
import kofre.dotted.{Dotted}
import kofre.time.{Dot, Dots}

object DeltaMultiValueRegister {
  type DeltaMultiValueRegisterLattice[V] = Dotted[Map[Dot, V]]

  def deltaWrite[V](
      value: V,
      replicaId: Uid,
      register: DeltaMultiValueRegisterLattice[V]
  ): DeltaMultiValueRegisterLattice[V] = {

    val dot = register.context.nextDot(replicaId)
    Dotted(
      Map(dot -> value),
      Dots.from(register.data.keySet + dot)
    )
  }

  def deltaClear[V: Lattice](register: DeltaMultiValueRegisterLattice[V]): DeltaMultiValueRegisterLattice[V] =
    Dotted(
      Map.empty,
      Dots.from(register.data.keySet)
    )

  def read[V](register: DeltaMultiValueRegisterLattice[V]): Set[V] = {
    register.data.values.toSet
  }
}
