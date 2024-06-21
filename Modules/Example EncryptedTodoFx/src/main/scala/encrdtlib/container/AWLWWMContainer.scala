package encrdtlib.container

import encrdtlib.container.AWLWWMContainer.LatticeType
import rdts.base.{Bottom, Uid}
import rdts.datatypes.LastWriterWins
import rdts.datatypes.contextual.ObserveRemoveMap
import rdts.dotted.Dotted
import rdts.syntax.LocalUid
import rdts.time.CausalTime

type AddWinsMapLattice[K, V] = Dotted[ObserveRemoveMap[K, V]]

class AWLWWMContainer[K, V](
    val replicaId: Uid,
    initialState: AddWinsMapLattice[K, LastWriterWins[V]] = Dotted(ObserveRemoveMap.empty[K, LastWriterWins[V]])
)(using Bottom[V]) {

  given LocalUid = replicaId

  private var _state: AddWinsMapLattice[K, LastWriterWins[V]] = initialState

  def state: LatticeType[K, V] = _state

  def put(key: K, value: V): Unit = {
    val timeStamp = _state.data.get(key) match {
      case Some(register) => register.timestamp.advance
      case None           => CausalTime.now()
    }

    _state = _state merge _state.mod(_.update(key, LastWriterWins(timeStamp, value)))
  }

}

object AWLWWMContainer {
  type LatticeType[K, V] = AddWinsMapLattice[K, LastWriterWins[V]]
}
