package probench.data

import channels.LatentConnection
import rdts.base.{Lattice, LocalUid}
import rdts.syntax.DeltaBuffer
import rdts.time.Dots
import replication.{ProtocolDots, ProtocolMessage, DataManager}

class ProDataManager[State: Lattice](
    val localReplicaId: LocalUid,
    val initialState: State,
    val onChange: (State, State) => Unit,
) {
  given Lattice[ProtocolDots[State]] = Lattice.derived
  private val dataManager            = DataManager[State](localReplicaId, _ => (), receivedChanges)

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

  def transform(fun: DeltaBuffer[State] => DeltaBuffer[State]): Unit = {
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
