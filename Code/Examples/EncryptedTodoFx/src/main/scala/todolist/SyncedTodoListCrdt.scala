package todolist

import com.github.plokhotnyuk.jsoniter_scala.core.{JsonReader, JsonValueCodec, JsonWriter}
import com.github.plokhotnyuk.jsoniter_scala.macros.{CodecMakerConfig, JsonCodecMaker}
import kofre.time.Dot
import kofre.encrdt.crdts.DeltaAddWinsLastWriterWinsMap
import kofre.primitives.LastWriterWins
import rescala.extra.encrdt.sync.ConnectionManager
import scalafx.application.Platform
import todolist.SyncedTodoListCrdt.StateType
import benchmarks.encrdt.Codecs.{ *, given}


import java.net.URI
import java.time.Instant
import java.util.UUID
import java.util.concurrent.{ExecutorService, Executors}
import scala.annotation.nowarn
import scala.concurrent.duration.{DurationInt, MILLISECONDS}
import scala.concurrent.{Await, ExecutionContext, Future}
import scala.util.Try
import benchmarks.encrdt.idFromString
class SyncedTodoListCrdt(val replicaId: String) {

  private val crdt: DeltaAddWinsLastWriterWinsMap[UUID, TodoEntry] =
    new DeltaAddWinsLastWriterWinsMap[UUID, TodoEntry](replicaId)

  private val crdtExecutorService: ExecutorService = Executors.newSingleThreadExecutor()
  private val crdtExecContext: ExecutionContext    = ExecutionContext.fromExecutor(crdtExecutorService)

  private val connectionManager: ConnectionManager[StateType] =
    ConnectionManagerFactory.connectionManager(replicaId, queryCrdtState, handleStateReceived)

  def address: URI = connectionManager.uri

  def connect(connectionString: String): Unit = {
    val parts = connectionString.split("@")
    if (parts.length == 2) {
      val uri = Try {
        URI.create(parts(1))
      }
      if (uri.isSuccess) {
        connectionManager.connectToReplica(parts(0), uri.get)
        return
      }
    }

    Console.err.println(s"Invalid connection string: $connectionString")
  }

  protected def handleStateReceived(state: StateType): Unit = {
    runInCrdtExecContext(() => {
      val before = crdt.values
      crdt.merge(state)
      val after = crdt.values
      TodoListController.handleUpdated(before, after)
    })
  }

  protected def queryCrdtState(): StateType = runInCrdtExecContext(() => crdt.state)

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
    ()
  }

  def get(key: UUID): Option[TodoEntry] =
    runInCrdtExecContext(() => crdt.get(key))

  def put(key: UUID, value: TodoEntry): Unit = {
    val newState = runInCrdtExecContext(() => {
      crdt.put(key, value)
      crdt.state
    })
    Platform.runLater {
      connectionManager.stateChanged(
        newState
      )
    }
  }

  def remove(key: UUID): Unit = {
    runInCrdtExecContext(() => {
      crdt.remove(key)
      connectionManager.stateChanged(crdt.state)
    })
  }

  def values: Map[UUID, TodoEntry] =
    runInCrdtExecContext(() => crdt.values)

  def remoteAddresses: Set[String] = connectionManager.remoteAddresses
}

object SyncedTodoListCrdt {
  type StateType = DeltaAddWinsLastWriterWinsMap.StateType[UUID, TodoEntry]

  private implicit val dotMapAsSetCodec: JsonValueCodec[Set[(Dot, (TodoEntry, LastWriterWins[Instant, String]))]] =
    JsonCodecMaker.make
  @nowarn
  private implicit val dotMapCodec: JsonValueCodec[Map[Dot, (TodoEntry, LastWriterWins[Instant, String])]] =
    new JsonValueCodec[Map[Dot, (TodoEntry, LastWriterWins[Instant, String])]] {
      override def decodeValue(
          in: JsonReader,
          default: Map[Dot, (TodoEntry, LastWriterWins[Instant, String])]
      ): Map[Dot, (TodoEntry, LastWriterWins[Instant, String])] =
        dotMapAsSetCodec.decodeValue(in, Set.empty).toMap

      override def encodeValue(x: Map[Dot, (TodoEntry, LastWriterWins[Instant, String])], out: JsonWriter): Unit =
        dotMapAsSetCodec.encodeValue(x.toSet, out)

      override def nullValue: Map[Dot, (TodoEntry, LastWriterWins[Instant, String])] =
        Map.empty
    }

  implicit val stateCodec: JsonValueCodec[StateType] =
    JsonCodecMaker.make(CodecMakerConfig.withAllowRecursiveTypes(true))
}
