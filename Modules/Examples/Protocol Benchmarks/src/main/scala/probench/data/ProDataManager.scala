package probench.data

import channels.LatentConnection
import rdts.base.{Lattice, LocalUid}
import rdts.syntax.DeltaBuffer
import replication.{DeltaDissemination, ProtocolMessage}

class ProDataManager[State: Lattice](
    localReplicaId: LocalUid,
    initialState: State,
    onChange: (State, State) => Unit,
    immediateForward: Boolean = false
) {
  private val dataManager =
    DeltaDissemination[State](localReplicaId, receivedChanges, immediateForward = immediateForward)

  var mergedState: State = initialState

  private def receivedChanges(changes: State): Unit = {
    val (o, n) = synchronized {
      val oldState = mergedState
      mergedState = oldState.merge(changes)
      (oldState, mergedState)
    }
    onChange(o, n)
  }

  def transform(fun: DeltaBuffer[State] => DeltaBuffer[State]): Unit = synchronized {
    val current: DeltaBuffer[State] = DeltaBuffer(mergedState)
    val next: DeltaBuffer[State]    = fun(current)

    next.deltaBuffer.foreach { delta =>
      dataManager.applyDelta(
        delta,
      )
      receivedChanges(delta)
    }
  }

  export dataManager.{addLatentConnection, pingAll}

}
