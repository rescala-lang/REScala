package de.ckuessner
package todolist

import encrdt.causality.DotStore.Dot
import encrdt.crdts.DeltaAddWinsLastWriterWinsMap
import encrdt.crdts.interfaces.MapCrdt
import encrdt.sync.ConnectionManager
import todolist.SyncedTodoListCrdt.{StateType, stateCodec}

import com.github.plokhotnyuk.jsoniter_scala.core.{JsonReader, JsonValueCodec, JsonWriter}
import com.github.plokhotnyuk.jsoniter_scala.macros.JsonCodecMaker

import java.net.URI
import java.time.Instant
import java.util.UUID
import java.util.concurrent.{ExecutorService, Executors}
import scala.concurrent.duration.{DurationInt, MILLISECONDS}
import scala.concurrent.{Await, ExecutionContext, Future}

class SyncedTodoListCrdt(val replicaId: String) extends MapCrdt[UUID, TodoEntry] {
  private val crdt: DeltaAddWinsLastWriterWinsMap[UUID, TodoEntry] = new DeltaAddWinsLastWriterWinsMap[UUID, TodoEntry](replicaId)

  private val crdtExecutorService: ExecutorService = Executors.newSingleThreadExecutor()
  private val crdtExecContext: ExecutionContext = ExecutionContext.fromExecutor(crdtExecutorService)

  private val connectionManager = new ConnectionManager[StateType](replicaId, queryCrdtState, handleStateReceived)

  def address: URI = connectionManager.uri

  def connect(connectionString: String): Unit = {
    val parts = connectionString.split("@")
    if (parts.length == 2) {
      connectionManager.connectToNewPeers(Map(parts(0) -> parts(1)))
    } else
      Console.err.println(s"Invalid connection string: $connectionString")
  }

  private def handleStateReceived(state: StateType): Unit = {
    crdtExecContext.execute(() => {
      val before = crdt.values
      crdt.merge(state)
      val after = crdt.values
      TodoListController.handleUpdated(before, after)
    })
  }

  private def queryCrdtState(): StateType = Await.result(
    Future {
      crdt.state
    }(crdtExecContext),
    1.seconds
  )

  private def runInCrdtExecContext[Ret](op: () => Ret): Ret = Await.result[Ret](
    Future {
      op()
    }(crdtExecContext),
    100.milliseconds
  )

  def shutdown(): Unit = {
    connectionManager.stop()
    crdtExecutorService.shutdown()
    crdtExecutorService.awaitTermination(500, MILLISECONDS)
  }

  override def get(key: UUID): Option[TodoEntry] =
    runInCrdtExecContext(() => crdt.get(key))

  override def put(key: UUID, value: TodoEntry): Unit = {
    runInCrdtExecContext(() => {
      crdt.put(key, value)
      connectionManager.stateChanged(crdt.state)
    })
  }

  override def remove(key: UUID): Unit = {
    runInCrdtExecContext(() => {
      crdt.remove(key)
      connectionManager.stateChanged(crdt.state)
    })
  }

  override def values: Map[UUID, TodoEntry] =
    runInCrdtExecContext(() => crdt.values)

  def peers: Map[String, String] = connectionManager.peers
}

object SyncedTodoListCrdt {
  type StateType = DeltaAddWinsLastWriterWinsMap.StateType[UUID, TodoEntry]

  private implicit val dotMapAsSetCodec: JsonValueCodec[Set[(Dot, (TodoEntry, (Instant, String)))]] = JsonCodecMaker.make
  private implicit val dotMapCodec: JsonValueCodec[Map[Dot, (TodoEntry, (Instant, String))]] = new JsonValueCodec[Map[Dot, (TodoEntry, (Instant, String))]] {
    override def decodeValue(in: JsonReader, default: Map[Dot, (TodoEntry, (Instant, String))]): Map[Dot, (TodoEntry, (Instant, String))] =
      dotMapAsSetCodec.decodeValue(in, Set.empty).toMap

    override def encodeValue(x: Map[Dot, (TodoEntry, (Instant, String))], out: JsonWriter): Unit =
      dotMapAsSetCodec.encodeValue(x.toSet, out)

    override def nullValue: Map[Dot, (TodoEntry, (Instant, String))] =
      Map.empty
  }

  implicit val stateCodec: JsonValueCodec[StateType] = JsonCodecMaker.make
}