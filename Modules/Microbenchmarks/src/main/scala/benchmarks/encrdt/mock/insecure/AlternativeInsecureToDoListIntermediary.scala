package benchmarks.encrdt.mock.insecure

import benchmarks.encrdt.localidFromString
import benchmarks.encrdt.mock.IntermediarySizeInfo
import benchmarks.encrdt.todolist.ToDoEntry
import com.github.plokhotnyuk.jsoniter_scala.core.{JsonValueCodec, readFromArray, writeToArray}
import rdts.syntax.DeltaAWLWWMContainer
import rdts.syntax.DeltaAWLWWMContainer.State

import java.util.UUID

class AlternativeInsecureToDoListIntermediary(val intermediaryReplicaId: String)(
    using stateJsonCodec: JsonValueCodec[State[UUID, ToDoEntry]]
) extends IntermediarySizeInfo {
  private val crdt = new DeltaAWLWWMContainer[UUID, ToDoEntry]("intermediary".convert)

  def receive(serializedDelta: Array[Byte]): Unit = {
    val delta: State[UUID, ToDoEntry] = readFromArray(serializedDelta)
    crdt.merge(delta)
  }

  def sizeInBytes: Long = writeToArray(crdt.state).length.toLong

  override val encDeltaCausalityInfoSizeInBytes: Long = 0L

  override def rawDeltasSizeInBytes: Long = sizeInBytes

  override def numberStoredDeltas: Int = 1
}
