package todolist

import rdts.syntax.LocalUid
import todolist.SyncedTodoListCrdt.{InnerStateType, StateType, given}
import todolist.sync.{ConnectionManager, DataManagerConnectionManager}

object ConnectionManagerFactory {
  var impl: (String, () => StateType, StateType => Unit) => ConnectionManager[StateType] =
    (replicaId, queryCrdtState, handleStateReceived) =>
      // new P2PConnectionManager[StateType](replicaId, queryCrdtState, handleStateReceived)
      DataManagerConnectionManager[InnerStateType](LocalUid.predefined(replicaId), handleStateReceived)

  def connectionManager(
      replicaId: String,
      query: () => StateType,
      stateReceived: StateType => Unit
  ): ConnectionManager[StateType] =
    impl(replicaId, query, stateReceived)
}


