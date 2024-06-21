package todolist

import channels.TCP
import com.github.plokhotnyuk.jsoniter_scala.core.JsonValueCodec
import encrdtlib.sync.ConnectionManager
import rdts.base.{Bottom, Lattice}
import rdts.dotted.Dotted
import rdts.syntax.LocalUid
import replication.{DataManager, ProtocolDots}
import todolist.SyncedTodoListCrdt.{InnerStateType, StateType, given}

import java.net.URI
import java.util.concurrent.{ExecutorService, Executors}
import scala.concurrent.ExecutionContext
import scala.util.Random
import scala.util.chaining.scalaUtilChainingOps

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

class DataManagerConnectionManager[State: JsonValueCodec: Lattice: Bottom](
    replicaId: LocalUid,
    receiveCallback: Dotted[State] => Unit
) extends ConnectionManager[Dotted[State]] {
  val dataManager: DataManager[State] =
    DataManager[State](replicaId: LocalUid, _ => (), pd => receiveCallback(Dotted(pd.data, pd.context)))

  val port = Random.nextInt(10000) + 50000

  val executor: ExecutorService = Executors.newCachedThreadPool((r: Runnable) =>
    Executors.defaultThreadFactory().newThread(r).tap(_.setDaemon(true))
  )
  val ec: ExecutionContext = ExecutionContext.fromExecutor(executor)

  dataManager.addLatentConnection(TCP.listen("127.0.0.1", port, ec))

  override val localReplicaId: String = replicaId.toString

  override def stateChanged(newState: Dotted[State]): Unit = {
    dataManager.applyLocalDelta(ProtocolDots(newState.data, newState.context))
  }

  override def connectToReplica(remoteReplicaId: String, uri: URI): Unit = {
    dataManager.addLatentConnection(TCP.connect(uri.getHost, uri.getPort, ec))
  }

  override def stop(): Unit = {
    dataManager.globalAbort.closeRequest = true
    executor.shutdownNow()
    ()
  }

  override def uri: URI = URI.create(s"tcp://127.0.0.1:$port")

  override def remoteAddresses: Set[String] = Set.empty
}
