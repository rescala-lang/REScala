package probench.data

import channels.LatentConnection
import rdts.base.{Lattice, LocalUid}
import rdts.syntax.DeltaBuffer
import rdts.time.Dots
import replication.{DataManager, ProtocolDots, ProtocolMessage}

class ProDataManager[State: Lattice](
    localReplicaId: LocalUid,
    initialState: State,
    onChange: (State, State) => Unit,
    immediateForward: Boolean = false
) {
  given Lattice[ProtocolDots[State]] = Lattice.derived
  private val dataManager =
    DataManager[State](localReplicaId, _ => (), receivedChanges, immediateForward = immediateForward)

  var mergedState: ProtocolDots[State] =
    dataManager.allDeltas.foldLeft(ProtocolDots(initialState, Dots.empty))(Lattice[ProtocolDots[State]].merge)

  private def receivedChanges(changes: ProtocolDots[State]): Unit = {
    val (o, n) = synchronized {
      val oldState = mergedState
      mergedState = oldState.merge(changes)
      (oldState, mergedState)
    }
    onChange(o.data, n.data)
  }

  def transform(fun: DeltaBuffer[State] => DeltaBuffer[State]): Unit = synchronized {
    val current: DeltaBuffer[State] = DeltaBuffer(mergedState.data)
    val next: DeltaBuffer[State]    = fun(current)

    next.deltaBuffer.foreach { delta =>
      dataManager.applyLocalDelta(ProtocolDots(
        delta,
        Dots.single(mergedState.context.nextDot(dataManager.replicaId.uid))
      ))
    }
  }

  export dataManager.{addLatentConnection, pingAll}

}
