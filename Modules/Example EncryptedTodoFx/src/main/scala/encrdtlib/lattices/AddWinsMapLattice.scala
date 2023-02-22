package encrdtlib.lattices

import kofre.base.{Lattice, Uid}
import kofre.datatypes.AddWinsSet
import kofre.dotted.Dotted
import kofre.time.Dots

case class AddWinsMapLattice[K, V](
    keys: Dotted[AddWinsSet[K]] = Dotted(AddWinsSet.empty[K]),
    mappings: Map[K, V] = Map[K, V]()
) {
  def values: Map[K, V] = mappings

  def added(key: K, value: V, replicaId: Uid): AddWinsMapLattice[K, V] = {
    val newKeys = keys merge keys.add(using replicaId)(key)
    val newMap  = mappings + (key -> value)
    AddWinsMapLattice(newKeys, newMap)
  }

  def removed(key: K): AddWinsMapLattice[K, V] = {
    val newKeys = keys merge keys.remove(key)
    val newMap  = mappings - key
    AddWinsMapLattice(newKeys, newMap)
  }
}

object AddWinsMapLattice {
  given AddWinsLattice[K, V: Lattice]: Lattice[AddWinsMapLattice[K, V]] = Lattice.derived
}
