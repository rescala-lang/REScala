package de.ckuessner
package todolist

import encrdt.sync.ConnectionManager
import encrdt.sync.p2p.P2PConnectionManager
import todolist.SyncedTodoListCrdt.{StateType, stateCodec}

object ConnectionManagerFactory {
  var impl: (String, () => StateType, StateType => Unit) => ConnectionManager[StateType] =
    (replicaId, queryCrdtState, handleStateReceived) =>
      new P2PConnectionManager[StateType](replicaId, queryCrdtState, handleStateReceived)

  def connectionManager(replicaId: String, query: () => StateType, stateReceived: StateType => Unit): ConnectionManager[StateType] =
    impl(replicaId, query, stateReceived)
}