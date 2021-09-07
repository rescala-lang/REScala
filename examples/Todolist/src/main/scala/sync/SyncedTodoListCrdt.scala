package de.ckuessner
package sync

import encrdt.crdts.AddWinsLastWriterWinsMap
import encrdt.crdts.interfaces.MapCrdt
import sync.SyncedTodoListCrdt.StateType
import todolist.{TodoEntry, TodoListController}

import com.github.plokhotnyuk.jsoniter_scala.core.JsonValueCodec
import com.github.plokhotnyuk.jsoniter_scala.macros.JsonCodecMaker

import java.util.UUID
import java.util.concurrent.{ExecutorService, Executors}
import scala.concurrent.duration.DurationInt
import scala.concurrent.{Await, ExecutionContext, Future}

class SyncedTodoListCrdt(val replicaId: String) extends MapCrdt[UUID, TodoEntry] {
  private val crdt: AddWinsLastWriterWinsMap[UUID, TodoEntry] = new AddWinsLastWriterWinsMap[UUID, TodoEntry](replicaId)
  private implicit val stateCodec: JsonValueCodec[StateType] = JsonCodecMaker.make

  private val crdtExecutorService: ExecutorService = Executors.newSingleThreadExecutor()
  private val crdtExecContext: ExecutionContext = ExecutionContext.fromExecutor(crdtExecutorService)

  private val syncHandler = new ConnectionHandler[StateType](
    replicaId,
    handleStateReceived,
    () => queryCrdtState
  )

  def address: String = syncHandler.address

  def connect(connectionString: String): Unit = {
    val parts = connectionString.split("@")
    if (parts.length == 2)
      syncHandler.connectWithClientWs(parts(0), parts(1))
    else
      Console.err.println("Invalid connection string: " + syncHandler)
  }

  private def handleStateReceived(state: StateType): Unit = {
    crdtExecContext.execute(() => {
      val before = crdt.values
      crdt.merge(state)
      val after = crdt.values
      TodoListController.handleUpdated(before, after)
    })
  }

  private def queryCrdtState: StateType = Await.result(
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
    syncHandler.shutdown()
    crdtExecutorService.shutdown()
  }

  override def get(key: UUID): Option[TodoEntry] =
    runInCrdtExecContext(() => crdt.get(key))

  override def put(key: UUID, value: TodoEntry): Unit = {
    runInCrdtExecContext(() => crdt.put(key, value))
    syncHandler.stateChanged()
  }

  override def remove(key: UUID): Unit = {
    runInCrdtExecContext(() => crdt.remove(key))
    syncHandler.stateChanged()
  }

  override def values: Map[UUID, TodoEntry] =
    runInCrdtExecContext(() => crdt.values)
}

object SyncedTodoListCrdt {
  type StateType = AddWinsLastWriterWinsMap.LatticeType[UUID, TodoEntry]
  implicit val latticeCodec: JsonValueCodec[StateType] = JsonCodecMaker.make
}