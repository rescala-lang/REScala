package benchmarks.encrdt.mock.insecure

import benchmarks.encrdt.mock.insecure.AlternativeInsecureToDoListClient.ToDoListState
import benchmarks.encrdt.mock.{DisseminationStats, ToDoListClient}
import benchmarks.encrdt.todolist.ToDoEntry
import com.github.plokhotnyuk.jsoniter_scala.core.{JsonValueCodec, writeToArray}
import rdts.syntax.DeltaAWLWWMContainer
import rdts.syntax.DeltaAWLWWMContainer.State

import java.util.UUID

class AlternativeInsecureToDoListClient(
    val replicaId: String,
    private val crdt: DeltaAWLWWMContainer[UUID, ToDoEntry],
    private val intermediary: AlternativeInsecureToDoListIntermediary
)(
    private implicit val stateCodec: JsonValueCodec[ToDoListState]
) extends ToDoListClient {

  override def completeToDoItem(uuid: UUID): Unit = {
    val _disseminatedBefore = _disseminatedDataInBytes
    val delta = crdt.putDelta(
      uuid,
      crdt.get(uuid).get.copy(completed = true)
    )

    disseminate(delta)

    _disseminatedDataCompletion += _disseminatedDataInBytes - _disseminatedBefore
  }

  override def addToDoItem(uuid: UUID, toDoEntry: ToDoEntry): Unit = {
    val _disseminatedBefore = _disseminatedDataInBytes
    val delta               = crdt.putDelta(uuid, toDoEntry)

    disseminate(delta)

    _disseminatedDataAddition += _disseminatedDataInBytes - _disseminatedBefore
  }

  override def removeToDoItems(uuids: Seq[UUID]): Unit = {
    val _disseminatedBefore = _disseminatedDataInBytes
    val delta               = crdt.removeAllDelta(uuids)

    disseminate(delta)

    _disseminatedDataRemoval += _disseminatedDataInBytes - _disseminatedBefore
  }

  private var _disseminatedDataInBytes: Long    = 0
  private var _disseminatedDataAddition: Long   = 0
  private var _disseminatedDataCompletion: Long = 0
  private var _disseminatedDataRemoval: Long    = 0

  override def disseminationStats: DisseminationStats = DisseminationStats(
    _disseminatedDataInBytes,
    _disseminatedDataAddition,
    _disseminatedDataCompletion,
    _disseminatedDataRemoval
  )

  def disseminate(delta: ToDoListState): Unit = {
    val serializedDelta: Array[Byte] = writeToArray(delta)
    _disseminatedDataInBytes += serializedDelta.length
    intermediary.receive(serializedDelta)
  }
}

private object AlternativeInsecureToDoListClient {
  type ToDoListState = State[UUID, ToDoEntry]
}
