package benchmarks.encrdt.mock.insecure

import benchmarks.encrdt.mock.IntermediarySizeInfo
import benchmarks.encrdt.todolist.ToDoEntry
import com.github.plokhotnyuk.jsoniter_scala.core.{JsonValueCodec, readFromArray, writeToArray}
import kofre.deprecated.containers.DeltaAddWinsLastWriterWinsMap.DeltaAddWinsLastWriterWinsMapLattice
import benchmarks.encrdt.idFromString
import kofre.deprecated.containers.DeltaAddWinsLastWriterWinsMap
import java.util.UUID

class AlternativeInsecureToDoListIntermediary(val intermediaryReplicaId: String)(
    implicit val stateJsonCodec: JsonValueCodec[DeltaAddWinsLastWriterWinsMapLattice[UUID, ToDoEntry]]
) extends IntermediarySizeInfo {
  private val crdt = new DeltaAddWinsLastWriterWinsMap[UUID, ToDoEntry]("intermediary")

  def receive(serializedDelta: Array[Byte]): Unit = {
    val delta: DeltaAddWinsLastWriterWinsMapLattice[UUID, ToDoEntry] = readFromArray(serializedDelta)
    crdt.merge(delta)
  }

  def sizeInBytes: Long = writeToArray(crdt.state).length.toLong

  override val encDeltaCausalityInfoSizeInBytes: Long = 0L

  override def rawDeltasSizeInBytes: Long = sizeInBytes

  override def numberStoredDeltas: Int = 1
}
