package encrdtlib.container

import encrdtlib.container.DeltaAWLWWMContainer.DeltaAddWinsLastWriterWinsMapLattice
import rdts.base.Lattice
import rdts.datatypes.LastWriterWins
import rdts.datatypes.contextual.ObserveRemoveMap
import rdts.datatypes.contextual.ObserveRemoveMap.Entry
import rdts.dotted.Dotted
import rdts.syntax.LocalUid
import rdts.time.Dots

class DeltaAWLWWMContainer[K, V](
    val replicaId: LocalUid,
    initialState: DeltaAddWinsLastWriterWinsMapLattice[K, V] = DeltaAWLWWMContainer.empty[K, V],
) {
  protected var _state: DeltaAddWinsLastWriterWinsMapLattice[K, V] = initialState

  def state: DeltaAddWinsLastWriterWinsMapLattice[K, V] = _state

  given LocalUid = replicaId

  def get(key: K): Option[V] =
    _state.data.get(key).map(_.value.value)

  def put(key: K, value: V): Unit = { putDelta(key, value); () }

  def putDelta(key: K, value: V): DeltaAddWinsLastWriterWinsMapLattice[K, V] = {
    val delta = {
      _state.mod { (context: Dots) ?=> (ormap: ObserveRemoveMap[K, Entry[LastWriterWins[V]]]) =>
        val nextDot = Dots.single(context.nextDot(replicaId.uid))
        ormap.transformPlain(key) {
          case Some(prior) => Some(Entry(nextDot, prior.value.write(value)))
          case None        => Some(Entry(nextDot, LastWriterWins.now(value)))
        }
      }
    }
    mutate(delta)
    delta
  }

  def remove(key: K): Unit = { removeDelta(key); () }

  def removeDelta(key: K): DeltaAddWinsLastWriterWinsMapLattice[K, V] = {
    val delta = {
      _state.mod { (context: Dots) ?=> (ormap: ObserveRemoveMap[K, Entry[LastWriterWins[V]]]) =>
        ormap.remove(key)
      }
    }
    mutate(delta)
    delta
  }

  def removeAllDelta(keys: Seq[K]): DeltaAddWinsLastWriterWinsMapLattice[K, V] = {
    val delta = _state.mod(orm => orm.clear())
    mutate(delta)
    delta
  }

  def values: Map[K, V] =
    _state.data.entries.map((k, v) => (k, v.value.value)).toMap

  def merge(other: DeltaAddWinsLastWriterWinsMapLattice[K, V]): Unit = {
    mutate(other)
  }

  private def mutate(delta: DeltaAddWinsLastWriterWinsMapLattice[K, V]): Unit = {
    _state = Dotted.lattice.merge(_state, delta)
  }
}

object DeltaAWLWWMContainer {
  type DeltaAddWinsLastWriterWinsMapLattice[K, V] = Dotted[InnerType[K, V]]

  def empty[K, V]: DeltaAddWinsLastWriterWinsMapLattice[K, V] =
    Dotted(ObserveRemoveMap.empty[K, Entry[LastWriterWins[V]]])

  type StateType[K, V] = DeltaAddWinsLastWriterWinsMapLattice[K, V]

  type InnerType[K, V] = ObserveRemoveMap[K, Entry[LastWriterWins[V]]]

  given deltaAddWinsMapLattice[K, V]: Lattice[DeltaAddWinsLastWriterWinsMapLattice[K, V]] = Dotted.lattice

}
