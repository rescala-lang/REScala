package todolist

import rdts.syntax.LocalUid
import todolist.SyncedTodoListCrdt.{InnerStateType, StateType, given}
import todolist.sync.{ConnectionManager, DataManagerConnectionManager}

object ConnectionManagerFactory {
  var impl: (LocalUid, () => StateType, StateType => Unit) => ConnectionManager[StateType] =
    (replicaId, queryCrdtState, handleStateReceived) =>
      // new P2PConnectionManager[StateType](replicaId, queryCrdtState, handleStateReceived)
      DataManagerConnectionManager[InnerStateType](replicaId, handleStateReceived)

  def connectionManager(
      replicaId: LocalUid,
      query: () => StateType,
      stateReceived: StateType => Unit
  ): ConnectionManager[StateType] =
    impl(replicaId, query, stateReceived)
}


