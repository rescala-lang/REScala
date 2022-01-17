
package encrdt.lattices
import kofre.Lattice
case class AddWinsMapLattice[K, V](keys: AddWinsSetLattice[K] = AddWinsSetLattice[K](), mappings: Map[K, V] = Map[K, V]()) {
  def values: Map[K, V] = mappings

  def added(key: K, value: V, replicaId: String): AddWinsMapLattice[K, V] = {
    val newKeys = keys.added(key, replicaId)
    val newMap = mappings + (key -> value)
    AddWinsMapLattice(newKeys, newMap)
  }

  def removed(key: K): AddWinsMapLattice[K, V] = {
    val newKeys = keys.removed(key)
    val newMap = mappings - key
    AddWinsMapLattice(newKeys, newMap)
  }
}

object AddWinsMapLattice {

  import OptionLattice.optLattice

  implicit def AddWinsLattice[K, V: Lattice]: Lattice[AddWinsMapLattice[K, V]] =
    (left: AddWinsMapLattice[K, V], right: AddWinsMapLattice[K, V]) => {
      val keys = Lattice.merge(left.keys, right.keys)
      val mappings = keys.values.map { key =>
        key -> Lattice.merge(left.mappings.get(key), right.mappings.get(key)).get
      }.toMap

      AddWinsMapLattice(keys, mappings)
    }
}
