package encrdtlib.container

import encrdtlib.container.AWLWWMContainer.LatticeType
import rdts.base.{Bottom, Lattice, Uid}
import rdts.datatypes.LastWriterWins
import rdts.datatypes.contextual.ObserveRemoveMap
import rdts.dotted.Dotted
import rdts.syntax.LocalUid
import rdts.time.CausalTime

type AddWinsMapLattice[K, V] = Dotted[ObserveRemoveMap[K, V]]

class AWLWWMContainer[K, V](
    val replicaId: Uid,
    initialState: AddWinsMapLattice[K, LastWriterWins[V]] =  Dotted(ObserveRemoveMap.empty[K, LastWriterWins[V]])
) {

  given LocalUid = replicaId


  private var _state: AddWinsMapLattice[K, LastWriterWins[V]] = initialState

  def state: LatticeType[K, V] = _state

  def get(key: K): Option[V] = _state.data.get(key).map(reg => reg.payload)

  def put(key: K, value: V)(using Bottom[V]): Unit = {
    val timeStamp = _state.data.get(key) match {
      case Some(register) => register.timestamp.advance
      case None           => CausalTime.now()
    }

    _state = _state.update(key, LastWriterWins(timeStamp, value))
  }

  def remove(key: K): Unit = _state = _state.remove(key)

  def values: Map[K, V] =
    _state.entries.map { case (k, LastWriterWins(_, v)) => k -> v }.toMap

  def merge(otherState: LatticeType[K, V])(using Bottom[V]): Unit = {
    _state = Lattice.merge(_state, otherState)
  }
}

object AWLWWMContainer {
  type LatticeType[K, V] = AddWinsMapLattice[K, LastWriterWins[V]]
}
