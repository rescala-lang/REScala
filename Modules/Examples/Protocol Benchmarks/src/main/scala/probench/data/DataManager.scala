package probench.data

import rdts.base.{Lattice, LocalUid}
import rdts.syntax.DeltaBuffer
import rdts.time.Dots
import replication.{ProtocolDots, DataManager as RepDataManager}

class DataManager[State: Lattice](
    val localReplicaId: LocalUid,
    val initialState: State,
    val onChange: (State, State) => Unit,
) {
  given Lattice[ProtocolDots[State]] = Lattice.derived
  private val dataManager = RepDataManager[State](localReplicaId, _ => (), receivedChanges)
  private var mergedState: ProtocolDots[State] = dataManager.allDeltas.foldLeft(ProtocolDots(initialState, Dots.empty))(Lattice[ProtocolDots[State]].merge)

  private def receivedChanges(changes: ProtocolDots[State]): Unit = {
    val oldState = mergedState
    mergedState = mergedState.merge(changes)

    onChange(oldState.data, mergedState.data)
  }

  def transform(fun: DeltaBuffer[State] => DeltaBuffer[State]): Unit = dataManager.lock.synchronized {
    val current: DeltaBuffer[State] = DeltaBuffer(mergedState.data)
    val next: DeltaBuffer[State]    = fun(current)
    
    next.deltaBuffer.foreach { delta =>
      dataManager.applyLocalDelta(ProtocolDots(
        delta,
        Dots.single(mergedState.context.nextDot(dataManager.replicaId.uid))
      ))
    }
  }

  export dataManager.addLatentConnection

}
